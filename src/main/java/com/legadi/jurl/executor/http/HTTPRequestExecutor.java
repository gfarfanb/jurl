package com.legadi.jurl.executor.http;

import static com.legadi.jurl.assertions.AssertionsResolver.evaluate;
import static com.legadi.jurl.common.CommonUtils.getOrDefault;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.WriterUtils.deleteFileFromPath;
import static com.legadi.jurl.common.WriterUtils.expandFile;
import static com.legadi.jurl.common.WriterUtils.printFile;
import static com.legadi.jurl.common.WriterUtils.writeFile;
import static com.legadi.jurl.common.WriterUtils.writeLine;
import static com.legadi.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;
import static java.util.logging.Level.FINE;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.CurlBuilder;
import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.URLBuilder;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.executor.RequestExecutor;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.AuthorizationType;
import com.legadi.jurl.model.RequestBehaviour;
import com.legadi.jurl.model.http.HTTPRequestAuthEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPRequestExecutor implements RequestExecutor<HTTPRequestEntry, HTTPResponseEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPRequestExecutor.class.getName());

    public static final String REQUEST_FILE_BOUNDARY = "http.request.executor/request.file.boundary";

    private static final String MULTIPART_LINE_END = "\r\n";
    private static final String MULTIPART_TWO_HYPHENS = "--";
    private static final int MULTIPART_MAX_BUFFER_SIZE = 1 * 1024 * 1024;
    private static final Pattern FILENAME_PATTERN = Pattern.compile(".*filename=(.*)");

    @Override
    public String name() {
        return "http";
    }

    @Override
    public TypeToken<HTTPRequestEntry> requestType() {
        return new TypeToken<HTTPRequestEntry>() {};
    }

    @Override
    public Optional<AssertionResult> acceptsConditions(Settings settings, HTTPRequestEntry request) {
        if(settings.isSkipConditions()) {
            return Optional.empty();
        }

        RequestBehaviour behaviour = settings.getRequestBehaviour();

        switch(behaviour) {
            case CURL_ONLY:
            case PRINT_ONLY:
                return Optional.empty();
            default:
                return evaluate(settings, request.getConditions());
        }
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, String requestPath, HTTPRequestEntry request)
            throws RequestException {
        CurlBuilder curlBuilder = new CurlBuilder();
        HttpURLConnection connection = createConnection(settings, request, curlBuilder);
        Path bodyPath = null;
        Path sentFilePath = null;

        if(request.getRequestFile() != null) {
            addFileHeaders(connection, settings, request, curlBuilder);
            addFileMethod(connection, request, curlBuilder);
            sentFilePath = sendFile(connection, settings, request, curlBuilder);
        } else {
            addHeaders(connection, settings, request, curlBuilder);
            addMethod(connection, request, curlBuilder);
            bodyPath = sendBody(connection, settings, requestPath, request, curlBuilder);
        }

        HTTPResponseEntry response = buildResponse(connection, settings, requestPath, request, curlBuilder);
        response.setBodyPath(bodyPath);
        response.setSentFilePath(sentFilePath);

        return response;
    }

    private HttpURLConnection createConnection(Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        URLBuilder urlBuilder = new URLBuilder()
            .setUrl(request.getUrl())
            .setProtocol(request.getProtocol())
            .setHost(request.getHost())
            .setPort(request.getPort())
            .setBasePath(request.getBasePath())
            .setEndpoint(request.getEndpoint())
            .addAllQueryParams(request.getQueryParams());
        String generatedUrl = null;

        try {
            generatedUrl = urlBuilder.build();
            URL url = new URL(generatedUrl);

            curlBuilder.setUrl(url);

            log(settings, identifyMethod(request) + " " + url, null);

            return createConnection(settings, request, url);
        } catch(IOException ex) {
            throw new RequestException(request, "Unable to create HTTP connection [" + generatedUrl + "] - " + ex.getMessage());
        }
    }

    private HttpURLConnection createConnection(Settings settings, HTTPRequestEntry request, URL url) throws IOException {
        if(settings.isMockRequest()) {
            return new HTTPMockConnection(url, request.getMockDefinition());
        }

        RequestBehaviour behaviour = settings.getRequestBehaviour();

        switch(behaviour) {
            case CURL_ONLY:
            case PRINT_ONLY:
                return new HTTPMockConnection(url, request.getMockDefinition());
            default:
                return (HttpURLConnection) url.openConnection();
        }
    }

    private void addFileMethod(HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        String method = identifyMethod(request);

        try {
            connection.setDoInput(true);
            connection.setDoOutput(true);
            connection.setUseCaches(false);
            connection.setRequestMethod(method);

            request.setMethod(method);
            curlBuilder.setMethod(method);
        } catch(ProtocolException ex) {
            throw new RequestException(request, "Invalid method for HTTP: " + method);
        }
    }

    private void addMethod(HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        String method = identifyMethod(request);

        try {
            boolean output;
            switch(method) {
                case "PUT":
                case "POST":
                case "PATCH":
                    output = true;
                    break;
                default:
                    output = false;
            }

            connection.setDoOutput(output);
            connection.setRequestMethod(method);

            curlBuilder.setMethod(method);
        } catch(ProtocolException ex) {
            throw new RequestException(request, "Invalid method for HTTP: " + method);
        }
    }

    private void addFileHeaders(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        String boundary = "*****" + Long.toString(System.currentTimeMillis()) + "*****";

        connection.setRequestProperty("Connection", "Keep-Alive");
        connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);

        request.getHeaders().put("Connection", "Keep-Alive");
        request.getHeaders().put("Content-Type", "multipart/form-data; boundary=" + boundary);

        settings.putOverride(REQUEST_FILE_BOUNDARY, boundary);

        addHeaders(connection, settings, request, curlBuilder);
    }

    private void addHeaders(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        StringBuilder printableHeaders = new StringBuilder();

        request.getHeaders()
            .entrySet()
            .stream()
            .filter(e -> isNotBlank(e.getKey()))
            .forEach(e -> {
                connection.setRequestProperty(e.getKey(), e.getValue());
                curlBuilder.addHeader(e.getKey(), e.getValue());
                printableHeaders.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
            });

        if(request.getRequestAuth() != null) {
            HTTPRequestAuthEntry auth = request.getRequestAuth();
            AuthorizationType authType = AuthorizationType.valueOfOrDefault(auth.getAuthType());

            switch(authType) {
                case BASIC:
                    String username = settings.get(auth.getUsernameParam());
                    String password = settings.get(auth.getPasswordParam());
                    String basicDecoded = username + ":" + password;
                    String basicValue = Base64.getEncoder().encodeToString(basicDecoded.getBytes());
                    connection.setRequestProperty("Authorization", "Basic " + basicValue);
                    curlBuilder.addHeader("Authorization", "Basic " + basicValue);
                    printableHeaders.append("Authorization").append(": Basic ").append(basicValue).append("\n");
                    break;
                case TOKEN:
                    String token = settings.get(auth.getTokenParam());
                    connection.setRequestProperty("Authorization", "Bearer " + token);
                    curlBuilder.addHeader("Authorization", "Bearer " + token);
                    printableHeaders.append("Authorization").append(": Bearer ").append(token).append("\n");
                    break;
                default:
                    LOGGER.fine("Authorization type not specified");
                    break;
            }
        }

        log(settings, printableHeaders.toString(), null);
    }

    private Path sendBody(HttpURLConnection connection, Settings settings, 
            String requestPath, HTTPRequestEntry request, CurlBuilder culrBuilder) {
        if(!connection.getDoOutput()) {
            return null;
        }

        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream())) {

            if(isNotBlank(request.getBodyContent())) {
                return sendBodyContent(dataOutputStream, settings, requestPath, request, culrBuilder);
            }

            if(isNotBlank(request.getBodyFilePath())) {
                return sendBodyFile(dataOutputStream, settings, requestPath, request, culrBuilder);
            }

            if(settings.containsOverride(BODY_TEMPORAL_PATH)) {
                return sendBodyTemporal(dataOutputStream, settings, request, culrBuilder);
            }

            return null;
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request body", ex);
        }
    }

    private Path sendBodyContent(DataOutputStream dataOutputStream, Settings settings, 
            String requestPath, HTTPRequestEntry request, CurlBuilder culrBuilder) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(requestPath)
            .setRequestName(request.getName())
            .setExtension("body");
        Path temporalBodyPath = pathBuilder.buildCommandPath();

        writeLine(dataOutputStream, request.getBodyContent(), request.getBodyCharset());
        writeFile(temporalBodyPath, request.getBodyContent());

        culrBuilder.setDataBinary(temporalBodyPath);

        log(settings, request.getBodyContent(), null);

        return temporalBodyPath;
    }

    private Path sendBodyFile(DataOutputStream dataOutputStream, Settings settings, 
            String requestPath, HTTPRequestEntry request, CurlBuilder culrBuilder) throws IOException {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(requestPath)
            .setRequestName(request.getName())
            .setExtension("body");
        Path temporalBodyPath = pathBuilder.buildCommandPath();
        Path requestBodyPath = Paths.get(request.getBodyFilePath());

        expandFile(settings, requestBodyPath, temporalBodyPath,
            line -> writeLine(dataOutputStream, line, request.getBodyCharset()));

        culrBuilder.setDataBinary(temporalBodyPath);

        log(settings, null, temporalBodyPath);

        return temporalBodyPath;
    }

    private Path sendBodyTemporal(DataOutputStream dataOutputStream, Settings settings, 
            HTTPRequestEntry request, CurlBuilder culrBuilder) throws IOException {
        Path bodyPath = Paths.get(settings.get(BODY_TEMPORAL_PATH));

        try(BufferedReader br = Files.newBufferedReader(bodyPath)) {
            br
                .lines()
                .forEach(line -> writeLine(dataOutputStream, line, request.getBodyCharset()));
        }

        culrBuilder.setDataBinary(bodyPath);

        log(settings, null, bodyPath);

        return bodyPath;
    }

    private Path sendFile(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        HTTPRequestFileEntry requestFile = validateAndGetRequestFile(request);
        String[] pathParts = requestFile.getPath().split("/");
        String filename = isNotBlank(requestFile.getName()) ? requestFile.getName() : pathParts[pathParts.length - 1];
        Map<String, String> formData = isNotEmpty(requestFile.getFormData()) ? requestFile.getFormData() : new HashMap<>();
        String boundary = settings.get(REQUEST_FILE_BOUNDARY);
        StringBuilder printableFile = new StringBuilder();

        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream());
                FileInputStream fileInputStream = new FileInputStream(requestFile.getPath())) {
            dataOutputStream.writeBytes(MULTIPART_TWO_HYPHENS + boundary + MULTIPART_LINE_END);
            dataOutputStream.writeBytes("Content-Disposition: form-data; name=\"" + requestFile.getField() + "\"; filename=\"" + filename + "\"" + MULTIPART_LINE_END);
            dataOutputStream.writeBytes("Content-Transfer-Encoding: binary" + MULTIPART_LINE_END);

            if(isNotBlank(requestFile.getMineType())) {
                dataOutputStream.writeBytes("Content-Type: " + requestFile.getMineType() + MULTIPART_LINE_END);
            }

            dataOutputStream.writeBytes(MULTIPART_LINE_END);

            int bytesAvailable = fileInputStream.available();
            int bufferSize = Math.min(bytesAvailable, MULTIPART_MAX_BUFFER_SIZE);
            byte[] buffer = new byte[bufferSize];
            int bytesRead = fileInputStream.read(buffer, 0, bufferSize);

            while (bytesRead > 0) {
                dataOutputStream.write(buffer, 0, bufferSize);
                bytesAvailable = fileInputStream.available();
                bufferSize = Math.min(bytesAvailable, MULTIPART_MAX_BUFFER_SIZE);
                bytesRead = fileInputStream.read(buffer, 0, bufferSize);
            }

            dataOutputStream.writeBytes(MULTIPART_LINE_END);

            for(Map.Entry<String, String> formEntry : formData.entrySet()) {
                dataOutputStream.writeBytes(MULTIPART_TWO_HYPHENS + boundary + MULTIPART_LINE_END);
                dataOutputStream.writeBytes("Content-Disposition: form-data; name=\"" + formEntry.getKey() + "\"" + MULTIPART_LINE_END);
                dataOutputStream.writeBytes("Content-Type: text/plain" + MULTIPART_LINE_END);
                dataOutputStream.writeBytes(MULTIPART_LINE_END);
                dataOutputStream.writeBytes(formEntry.getValue());
                dataOutputStream.writeBytes(MULTIPART_LINE_END);
            };

            dataOutputStream.writeBytes(MULTIPART_TWO_HYPHENS + boundary + MULTIPART_TWO_HYPHENS + MULTIPART_LINE_END);

            curlBuilder.setFile(requestFile.getField(), requestFile.getPath(), filename, requestFile.getMineType());

            printableFile.append(requestFile.getField()).append("=").append(requestFile.getPath()).append("\n");
            printableFile.append("filename=").append(requestFile.getPath()).append("\n");

            if(isNotBlank(requestFile.getMineType())) {
                printableFile.append("type=").append(requestFile.getMineType()).append("\n");
            }

            formData.forEach((field, value) -> {
                curlBuilder.addForm(field, value);
                printableFile.append(field).append("=").append(value).append("\n");
            });

            log(settings, printableFile.toString(), null);

            return Paths.get(requestFile.getPath());
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request file: " + requestFile.getPath(), ex);
        }
    }

    private String getOutputFilename(HTTPResponseEntry response) {
        String contentDisposition = response.getResponseHeaders()
            .entrySet()
            .stream()
            .filter(e -> "Content-Disposition".equalsIgnoreCase(e.getKey()))
            .map(Map.Entry::getValue)
            .findFirst()
            .orElse(null);

            if(isBlank(contentDisposition)) {
                return null;
            }

            Matcher matcher = FILENAME_PATTERN.matcher(contentDisposition);

            if(matcher.find()) {
                return strip(matcher.group(1), "\"");
            } else {
                return null;
            }
    }

    private Path readOutput(HttpURLConnection connection, Settings settings,
            String requestPath, HTTPRequestEntry request, String filename) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath(requestPath)
                .setRequestName(request.getName())
                .setFilename(filename)
                .setExtension(isBlank(filename) ? "response" : null);
        Path responsePath = pathBuilder.buildCommandPath();

        if(readOutput(true, connection, responsePath)) {
            return responsePath;
        } else if(readOutput(false, connection, responsePath)) {
            return responsePath;
        } else {
            return null;
        }
    }

    private boolean readOutput(boolean requireConnectionStream, HttpURLConnection connection, Path responsePath) {
        String streamName = requireConnectionStream ? "connection-stream" : "error-stream";
        boolean wasOutputWritten = false;

        try(InputStream inputStream = requireConnectionStream 
                ? connection.getInputStream() : connection.getErrorStream();
                OutputStream outputStream = Files.newOutputStream(responsePath)) {

            if(inputStream == null) {
                LOGGER.fine("'" + streamName + "' is null");
                deleteFileFromPath(responsePath);
                return false;
            }

            byte[] buffer = new byte[8 * 1024];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
                wasOutputWritten = true;
            }

            if(!wasOutputWritten) {
                deleteFileFromPath(responsePath);
            }

            return wasOutputWritten;
        } catch(IOException ex) {
            LOGGER.log(FINE, "Error on reading response from '" + streamName + "'", ex);
            deleteFileFromPath(responsePath);
            return false;
        }
    }

    private HTTPResponseEntry buildResponse(HttpURLConnection connection,
            Settings settings, String requestPath, HTTPRequestEntry request,
            CurlBuilder curlBuilder) {
        HTTPResponseEntry response = new HTTPResponseEntry();
        StringBuilder printableResponse = new StringBuilder();

        try {
            response.setRequestUrl(connection.getURL().toString());
            response.setCurlCommand(curlBuilder.build());
            response.setStatusCode(connection.getResponseCode());
            response.setResponseHeaders(connection.getHeaderFields()
                .entrySet()
                .stream()
                .collect(Collectors.toMap(
                    e -> getOrDefault(e.getKey(), "HTTP:HEADER:RESULT"),
                    e -> String.join(",", e.getValue()),
                    (v1, v2) -> v2,
                    HashMap::new
                ))
            );
            response.setResult(response.getResponseHeaders().remove("HTTP:HEADER:RESULT"));

            String filename = getOutputFilename(response);
            Path responsePath = readOutput(connection, settings, requestPath, request, filename);

            response.setResponsePath(responsePath);

            printableResponse.append(response.getResult()).append("\n");
            response.getResponseHeaders()
                .forEach((header, value) ->
                    printableResponse.append(header).append(": ").append(value).append("\n"));

            log(settings, printableResponse.toString(), null);

            return response;
        } catch(IOException ex) {
            throw new IllegalStateException("Error on creating response", ex);
        }
    }

    private HTTPRequestFileEntry validateAndGetRequestFile(HTTPRequestEntry request) {
        HTTPRequestFileEntry requestFile = request.getRequestFile();

        if(isBlank(requestFile.getPath())) {
            throw new RequestException(request, "request.requestFile.path (file path) is null or empty");
        }
        if(isBlank(requestFile.getField())) {
            throw new RequestException(request, "request.requestFile.field (form file field) is null or empty");
        }
        
        return requestFile;
    }

    private String identifyMethod(HTTPRequestEntry request) {
        if(isNotBlank(request.getMethod())) {
            return request.getMethod();
        }
        if(request.getRequestFile() != null) {
            return "POST";
        }
        throw new RequestException(request, "HTTP method not defined");
    }

    private void log(Settings settings, String entry, Path filePath) {
        RequestBehaviour behaviour = settings.getRequestBehaviour();

        switch(behaviour) {
            case PRINT_ONLY:
            case REQUEST:
                if(isNotBlank(entry)) {
                    LOGGER.info(entry);
                }
                if(filePath != null) {
                    printFile(filePath);
                }
                return;
            default:
                return;
        }
    }
}
