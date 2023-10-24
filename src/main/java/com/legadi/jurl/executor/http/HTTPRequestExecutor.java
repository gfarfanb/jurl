package com.legadi.jurl.executor.http;

import static com.legadi.jurl.assertions.AssertionsResolver.evaluate;
import static com.legadi.jurl.common.CommonUtils.getOrDefault;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.RequestUtils.mergeRequestHeader;
import static com.legadi.jurl.common.WriterUtils.expandFile;
import static com.legadi.jurl.common.WriterUtils.printFile;
import static com.legadi.jurl.common.WriterUtils.writeLine;
import static com.legadi.jurl.executor.mixer.BodyMixerRegistry.findByBodyType;
import static com.legadi.jurl.parser.RequestParserRegistry.findByRequestType;
import static java.util.logging.Level.FINE;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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
import com.legadi.jurl.executor.mixer.BodyMixer;
import com.legadi.jurl.executor.mixer.BodyMixer.MixerEntry;
import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.Credential;
import com.legadi.jurl.model.RequestBehaviour;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;
import com.legadi.jurl.parser.HTTPRequestParser;

public class HTTPRequestExecutor implements RequestExecutor<HTTPRequestEntry, HTTPResponseEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPRequestExecutor.class.getName());

    public static final String BODY_TEMPORAL_PATH = "http.request.executor/body.temporal.path";
    public static final String REQUEST_FILE_BOUNDARY = "http.request.executor/request.file.boundary";

    private static final String MULTIPART_LINE_END = "\r\n";
    private static final String MULTIPART_TWO_HYPHENS = "--";
    private static final int MULTIPART_MAX_BUFFER_SIZE = 1 * 1024 * 1024;
    private static final Pattern FILENAME_PATTERN = Pattern.compile(".*filename=(.*)");

    @Override
    public String type() {
        return "http";
    }

    @Override
    public TypeToken<HTTPRequestEntry> requestType() {
        return new TypeToken<HTTPRequestEntry>() {};
    }

    @Override
    public boolean acceptsConditions(Settings settings, HTTPRequestEntry request) {
        if(settings.isSkipConditions()) {
            return true;
        }

        RequestBehaviour behaviour = settings.getRequestBehaviour();

        switch(behaviour) {
            case CURL_ONLY:
            case PRINT_ONLY:
                return true;
            default:
                return evaluate(settings, request.getConditions())
                    .map(AssertionResult::isPassed)
                    .orElse(true);
        }
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, String requestPath, HTTPRequestEntry request)
            throws RequestException {
        CurlBuilder curlBuilder = new CurlBuilder();
        HttpURLConnection connection = createConnection(settings, request, curlBuilder);

        if(request.getRequestFile() != null) {
            addFileHeaders(connection, settings, request, curlBuilder);
            addFileMethod(connection, request, curlBuilder);
            sendFile(connection, settings, request, curlBuilder);
        } else {
            addHeaders(connection, settings, request, curlBuilder);
            addMethod(connection, request, curlBuilder);
            sendBody(connection, settings, requestPath, request, curlBuilder);
        }

        return buildResponse(connection, settings, requestPath, request, curlBuilder);
    }

    @Override
    public void mergeAPIDefinition(Settings settings, HTTPRequestEntry api, HTTPRequestEntry request) {
        mergeRequestHeader(api, request);

        if(isBlank(request.getMethod())) {
            request.setMethod(api.getMethod());
        }

        Map<String, String> queryParams = new HashMap<>(api.getQueryParams());
        queryParams.putAll(request.getQueryParams());
        request.setQueryParams(queryParams);

        Map<String, String> headers = new HashMap<>(api.getHeaders());
        headers.putAll(request.getHeaders());
        request.setHeaders(headers);

        if(isBlank(request.getBodyCharset())) {
            request.setBodyCharset(api.getBodyCharset());
        }
        if(isBlank(request.getBodyContent())) {
            request.setBodyContent(api.getBodyContent());
        }
        if(isBlank(request.getBodyFilePath())) {
            request.setBodyFilePath(api.getBodyFilePath());
        }

        if(request.getRequestFile() == null) {
            request.setRequestFile(api.getRequestFile());
        } else if(api.getRequestFile() != null) {
            mergeRequestFile(api.getRequestFile(), request.getRequestFile());
        }

        Map<String, String> outputMappings = new HashMap<>(api.getOutputMappings());
        outputMappings.putAll(request.getOutputMappings());
        request.setOutputMappings(outputMappings);

        List<AssertionEntry> assertions = new LinkedList<>(api.getAssertions());
        assertions.addAll(request.getAssertions());
        request.setAssertions(assertions);
    }

    @Override
    public void mergeBodyFileWithBodyContent(Settings settings, String requestPath, HTTPRequestEntry request) {
        if(isBlank(request.getBodyFilePath())) {
            throw new RequestException(request, "request.bodyFilePath is null or empty");
        }
        if(isBlank(request.getBodyContent())) {
            throw new RequestException(request, "request.bodyContent is null or empty");
        }

        BodyMixer mixer = findByBodyType(settings.getMergeBodyUsingType());
        Path bodyTemporalPath = mixer.apply(settings, new MixerEntry()
            .setRequestPath(requestPath)
            .setRequestName(request.getName())
            .setBodyFilePath(request.getBodyFilePath())
            .setBodyContent(request.getBodyContent()));

        request.setBodyContent(null);
        request.setBodyFilePath(null);

        settings.putOverride(BODY_TEMPORAL_PATH, bodyTemporalPath.toString());
    }

    @Override
    public void overrideRequestWithFile(Settings settings, HTTPRequestEntry request, String filename) {
        HTTPRequestParser parser = findByRequestType(type());
        HTTPRequestEntry overrideRequest = parser.parseRequest(settings, Paths.get(filename));

        if(isNotEmpty(overrideRequest.getHeaders())) {
            request.setHeaders(overrideRequest.getHeaders());
        }

        if(isNotEmpty(overrideRequest.getQueryParams())) {
            request.setQueryParams(overrideRequest.getQueryParams());
        }

        if(isNotEmpty(overrideRequest.getAssertions())) {
            request.setAssertions(overrideRequest.getAssertions());
        }

        if(isNotEmpty(overrideRequest.getOutputMappings())) {
            request.setOutputMappings(overrideRequest.getOutputMappings());
        }

        if(isNotBlank(overrideRequest.getBodyContent())) {
            request.setBodyContent(overrideRequest.getBodyContent());
        }

        if(isNotBlank(overrideRequest.getBodyFilePath())) {
            request.setBodyFilePath(overrideRequest.getBodyFilePath());
        }
    }

    private void mergeRequestFile(HTTPRequestFileEntry api, HTTPRequestFileEntry request) {
        if(isBlank(request.getName())) {
            request.setName(api.getName());
        }
        if(isBlank(request.getPath())) {
            request.setPath(api.getPath());
        }
        if(isBlank(request.getField())) {
            request.setField(api.getField());
        }
        if(isBlank(request.getMineType())) {
            request.setMineType(api.getMineType());
        }

        Map<String, String> formData = new HashMap<>(api.getFormData());
        formData.putAll(request.getFormData());
        request.setFormData(formData);
    }

    private HttpURLConnection createConnection(Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        URLBuilder urlBuilder = new URLBuilder()
            .setUrl(request.getUrl())
            .setProtocol(request.getProtocol())
            .setDomain(request.getDomain())
            .setPort(request.getPort())
            .setBasePath(request.getBasePath())
            .setEndpoint(request.getEndpoint())
            .addAllQueryParams(request.getQueryParams());

        try {
            URL url = new URL(urlBuilder.build());

            curlBuilder.setUrl(url);

            log(settings, identifyMethod(request) + " " + url, null);

            return createConnection(settings, request, url);
        } catch(MalformedURLException ex) {
            throw new RequestException(request, "Malformed HTTP resource: " + request.getUrl());
        } catch(IOException ex) {
            throw new RequestException(request, "Unable to create HTTP connection: " + request.getUrl());
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

        request.getHeaders().forEach(
            (header, value) -> {
                connection.setRequestProperty(header, value);
                curlBuilder.addHeader(header, value);
                printableHeaders.append(header).append(": ").append(value).append("\n");
            }
        );

        switch(settings.getAuthorizationType()) {
            case BASIC:
                Credential basicCredential = settings.getCredential();
                String basicDecoded = basicCredential.getUsername() + ":" + basicCredential.getPassword();
                String basicValue = Base64.getEncoder().encodeToString(basicDecoded.getBytes());
                connection.setRequestProperty("Authorization", "Basic " + basicValue);
                curlBuilder.addHeader("Authorization", "Basic " + basicValue);
                printableHeaders.append("Authorization").append(": Basic ").append(basicValue).append("\n");
                break;
            case TOKEN:
                Credential tokenCredential = settings.getCredential();
                connection.setRequestProperty("Authorization", "Bearer " + tokenCredential.getToken());
                curlBuilder.addHeader("Authorization", "Bearer " + tokenCredential.getToken());
                printableHeaders.append("Authorization").append(": Bearer ").append(tokenCredential.getToken()).append("\n");
                break;
            default:
                LOGGER.fine("Authorization type not specified");
                break;
        }

        log(settings, printableHeaders.toString(), null);
    }

    private void sendBody(HttpURLConnection connection, Settings settings, 
            String requestPath, HTTPRequestEntry request, CurlBuilder culrBuilder) {
        if(!connection.getDoOutput()) {
            return;
        }

        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream())) {

            if(isNotBlank(request.getBodyContent())) {
                sendBodyContent(dataOutputStream, settings, request, culrBuilder);
                return;
            }

            if(isNotBlank(request.getBodyFilePath())) {
                sendBodyFile(dataOutputStream, settings, requestPath, request, culrBuilder);
                return;
            }

            if(settings.containsOverride(BODY_TEMPORAL_PATH)) {
                sendBodyTemporal(dataOutputStream, settings, request, culrBuilder);
                return;
            }

            throw new RequestException(request, "request.bodyContent or request.bodyFilePath is null or empty");
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request body", ex);
        }
    }

    private void sendBodyContent(DataOutputStream dataOutputStream, Settings settings, 
            HTTPRequestEntry request, CurlBuilder culrBuilder) {
        writeLine(dataOutputStream, request.getBodyContent(), request.getBodyCharset());
        culrBuilder.setData(request.getBodyContent());
        log(settings, request.getBodyContent(), null);
    }

    private void sendBodyFile(DataOutputStream dataOutputStream, Settings settings, 
            String requestPath, HTTPRequestEntry request, CurlBuilder culrBuilder) throws IOException {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(requestPath)
            .setRequestName(request.getName())
            .setExtension("body");
        Path temporalBodyPath = pathBuilder.buildCommandPath();
        Path requestBodyPath = Paths.get(request.getBodyFilePath());

        expandFile(settings, requestBodyPath, temporalBodyPath,
            line -> writeLine(dataOutputStream, line, request.getBodyCharset()));

        culrBuilder.setDataBinary(temporalBodyPath.toString());

        log(settings, null, temporalBodyPath);
    }

    private void sendBodyTemporal(DataOutputStream dataOutputStream, Settings settings, 
            HTTPRequestEntry request, CurlBuilder culrBuilder) throws IOException {
        Path bodyPath = Paths.get(settings.get(BODY_TEMPORAL_PATH));

        try(BufferedReader br = Files.newBufferedReader(bodyPath)) {
            br
                .lines()
                .forEach(line -> writeLine(dataOutputStream, line, request.getBodyCharset()));
        }

        culrBuilder.setDataBinary(bodyPath.toString());

        log(settings, null, bodyPath);
    }

    private void sendFile(HttpURLConnection connection, Settings settings,
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
        boolean wasOutputWritten = false;

        try(InputStream inputStream = connection.getInputStream();
                OutputStream outputStream = Files.newOutputStream(responsePath)) {

            byte[] buffer = new byte[8 * 1024];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
                wasOutputWritten = true;
            }
        } catch(IOException ex) {
            LOGGER.log(FINE, "Error on reading response", ex);
            return null;
        }

        if(wasOutputWritten) {
            return responsePath;
        } else {
            responsePath.toFile().delete();
            return null;
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
        if(request.getRequestFile() != null) {
            return "POST";
        }
        if(isNotBlank(request.getMethod())) {
            return request.getMethod();
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
