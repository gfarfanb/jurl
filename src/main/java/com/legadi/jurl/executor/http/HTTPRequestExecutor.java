package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.LoaderUtils.printFile;
import static java.util.logging.Level.FINE;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.HashMap;
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
import com.legadi.jurl.model.Credential;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class HTTPRequestExecutor implements RequestExecutor<HTTPRequestEntry, HTTPResponseEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPRequestExecutor.class.getName());

    private static final String MULTIPART_LINE_END = "\r\n";
    private static final String MULTIPART_TWO_HYPHENS = "--";
    private static final int MULTIPART_MAX_BUFFER_SIZE = 1 * 1024 * 1024;
    private static final String FILENAME_PATTERN = ".*filename=(.*)";

    @Override
    public boolean accepts(RequestEntry request) {
        return (isNotBlank(request.getUrl()) && request.getUrl().startsWith("http"))
            || (isNotBlank(request.getProtocol()) && request.getProtocol().startsWith("http"));
    }

    @Override
    public TypeToken<HTTPRequestEntry> type() {
        return new TypeToken<HTTPRequestEntry>() {};
    }

    @Override
    public HTTPResponseEntry executeRequest(Settings settings, HTTPRequestEntry request) throws RequestException {
        CurlBuilder curlBuilder = new CurlBuilder();
        HttpURLConnection connection = createConnection(settings, request, curlBuilder);

        addHeaders(connection, settings, request, curlBuilder);

        if(request.getRequestFile() != null) {
            sendFile(connection, settings, request, curlBuilder);
        } else {
            addMethod(connection, request, curlBuilder);
            sendBody(connection, settings, request, curlBuilder);
        }

        return buildResponse(connection, settings, request, curlBuilder);
    }

    @Override
    public void overrideRequest(Settings settings, HTTPRequestEntry request, String filename) {
        HTTPRequestParser parser = new HTTPRequestParser(settings);
        HTTPRequestEntry overrideRequest = parser.parseRequest(request.getRequestPath(), request.getName(), filename);

        if(isNotEmpty(request.getHeaders())) {
            request.getHeaders().putAll(overrideRequest.getHeaders());
        } else {
            request.setHeaders(overrideRequest.getHeaders());
        }

        if(isNotEmpty(request.getQueryParams())) {
            request.getQueryParams().putAll(overrideRequest.getQueryParams());
        } else {
            request.setQueryParams(overrideRequest.getQueryParams());
        }

        if(isNotEmpty(overrideRequest.getAssertions())) {
            request.setAssertions(overrideRequest.getAssertions());
        }

        if(isNotEmpty(overrideRequest.getOutputMappings())) {
            request.setOutputMappings(overrideRequest.getOutputMappings());
        }

        if(isNotBlank(overrideRequest.getBodyFilePath())) {
            request.setBodyContent(null);
            request.setBodyFilePath(overrideRequest.getBodyFilePath());
        }
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

            if(!settings.isCurlRequest()) {
                LOGGER.info(request.getMethod() + " " + url);
            }

            return createConnection(settings, request, url);
        } catch(MalformedURLException ex) {
            throw new RequestException(request, "Malformed HTTP resource: " + request.getUrl());
        } catch(IOException ex) {
            throw new RequestException(request, "Unable to create HTTP connection: " + request.getUrl());
        }
    }

    private HttpURLConnection createConnection(Settings settings, HTTPRequestEntry request, URL url) throws IOException {
        if(isNotBlank(settings.getMockRequestClass())) {
            try {
                Class<?> connectionClass = Class.forName(settings.getMockRequestClass());
                Constructor<?> constructor = connectionClass.getConstructor();
                return (HttpURLConnection) constructor.newInstance();
            } catch(Exception ex) {
                throw new RequestException(request, "Invalid mock definition class: " + settings.getMockRequestClass());
            }
        }

        if(settings.isCurlRequest() || settings.isMockRequest()) {
            return new HTTPMockConnection(url);
        }

        return (HttpURLConnection) url.openConnection();
    }

    private void addMethod(HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        if(isBlank(request.getMethod())) {
            throw new RequestException(request, "HTTP method not defined");
        }

        try {
            boolean output;
            switch(request.getMethod()) {
                case "PUT":
                case "POST":
                    output = true;
                    break;
                default:
                    output = false;
            }

            connection.setDoOutput(output);
            connection.setRequestMethod(request.getMethod());

            curlBuilder.setMethod(request.getMethod());
        } catch(ProtocolException ex) {
            throw new RequestException(request, "Invalid method for HTTP: " + request.getMethod());
        }
    }

    private void addHeaders(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        if(isEmpty(request.getHeaders())) {
            return;
        }

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

        if(!settings.isCurlRequest()) {
            LOGGER.info(printableHeaders.toString());
        }
    }

    private void sendBody(HttpURLConnection connection, Settings settings, 
            HTTPRequestEntry request, CurlBuilder culrBuilder) {
        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream())) {

            if(isNotBlank(request.getBodyContent())) {
                writeLine(dataOutputStream, request.getBodyContent(), request.getBodyCharset());
                culrBuilder.setData(request.getBodyContent());

                if(!settings.isCurlRequest()) {
                    LOGGER.info(request.getBodyContent());
                }

                return;
            }

            if(isNotBlank(request.getBodyFilePath())) {
                Path bodyPath = Paths.get(request.getBodyFilePath());

                try(BufferedReader br = Files.newBufferedReader(bodyPath)) {
                    br.lines().forEach(line -> writeLine(dataOutputStream, line, request.getBodyCharset()));
                }

                culrBuilder.setDataBinary(request.getBodyFilePath());

                if(!settings.isCurlRequest()) {
                    printFile(request.getBodyFilePath());
                }

                return;
            }

            throw new RequestException(request, "request.bodyContent or request.bodyFile is null or empty");
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request body", ex);
        }
    }

    private void sendFile(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        HTTPRequestFileEntry requestFile = validateAndGetRequestFile(request);
        String boundary = "*****" + Long.toString(System.currentTimeMillis()) + "*****";
        String[] pathParts = requestFile.getPath().split("/");
        String filename = isNotBlank(requestFile.getName()) ? requestFile.getName() : pathParts[pathParts.length - 1];
        Map<String, String> formData = isNotEmpty(requestFile.getFormData()) ? requestFile.getFormData() : new HashMap<>();
        StringBuilder printableFile = new StringBuilder();

        connection.setDoInput(true);
        connection.setDoOutput(true);
        connection.setUseCaches(false);
        connection.setRequestProperty("Connection", "Keep-Alive");
        connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);

        try {
            connection.setRequestMethod("POST");
        } catch(ProtocolException ex) {
            throw new RequestException(request, "Invalid method for HTTP: " + request.getMethod());
        }

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

            curlBuilder.setMethod("POST");
            curlBuilder.setFile(requestFile.getField(), requestFile.getPath(), filename, requestFile.getMineType());

            printableFile.append(requestFile.getField()).append("=").append(requestFile.getPath()).append("\n");
            if(isNotBlank(filename)) {
                printableFile.append("filename=").append(requestFile.getPath()).append("\n");
            }
            if(isNotBlank(requestFile.getMineType())) {
                printableFile.append("type=").append(requestFile.getMineType()).append("\n");
            }

            formData.forEach((field, value) -> {
                curlBuilder.addForm(field, value);
                printableFile.append(field).append("=").append(value).append("\n");
            });

            if(!settings.isCurlRequest()) {
                LOGGER.info(printableFile.toString());

            }
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

            Pattern pattern = Pattern.compile(FILENAME_PATTERN);
            Matcher matcher = pattern.matcher(contentDisposition);

            if(matcher.find()) {
                return strip(matcher.group(0), "\"");
            } else {
                return null;
            }
    }

    private Path readOutput(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, String filename) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
                .setRequestPath(request.getRequestPath())
                .setRequestName(request.getName())
                .setFilename(filename)
                .setExtension(isBlank(filename) ? "response" : null);
        Path responsePath = pathBuilder.buildOutputPath();

        try(BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                FileWriter responseWriter = new FileWriter(responsePath.toFile())) {

            String line;
            while((line = br.readLine()) != null) {
                responseWriter.write(line);
            }

            return responsePath;
        } catch(IOException ex) {
            LOGGER.log(FINE, "Error on reading response", ex);
            return null;
        }
    }

    private HTTPResponseEntry buildResponse(HttpURLConnection connection,
            Settings settings, HTTPRequestEntry request, CurlBuilder curlBuilder) {
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
                    e -> e.getKey(),
                    e -> String.join(",", e.getValue()),
                    (v1, v2) -> v2,
                    HashMap::new
                ))
            );

            String filename = getOutputFilename(response);
            Path responsePath = readOutput(connection, settings, request, filename);

            response.setResponsePath(responsePath);

            printableResponse.append("HTTP/ ").append(response.getStatusCode()).append("\n");
            response.getResponseHeaders()
                .forEach((header, value) ->
                    printableResponse.append(header).append(": ").append(value).append("\n"));

            if(!settings.isCurlRequest()) {
                LOGGER.info(printableResponse.toString());
            }

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

    private void writeLine(DataOutputStream dataOutputStream, String line, String charset) {
        try {
            byte[] input = line.getBytes(charset);
            dataOutputStream.write(input, 0, input.length);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write line", ex);
        }
    }
}
