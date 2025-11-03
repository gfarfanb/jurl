package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.assertions.AssertionsResolver.evaluate;
import static com.legadi.cli.jurl.common.Command.exec;
import static com.legadi.cli.jurl.common.CommonUtils.getOrDefault;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.strip;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findAll;
import static com.legadi.cli.jurl.common.WriterUtils.deleteFileFromPath;
import static com.legadi.cli.jurl.common.WriterUtils.expandFile;
import static com.legadi.cli.jurl.common.WriterUtils.printFile;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;
import static com.legadi.cli.jurl.common.WriterUtils.writeLine;
import static com.legadi.cli.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.CurlBuilder;
import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.URLBuilder;
import com.legadi.cli.jurl.exception.RequestException;
import com.legadi.cli.jurl.executor.HeaderAuthenticator;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.RequestBehaviour;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPRequestFileEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

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
    public HTTPResponseEntry executeRequest(Settings settings, String requestInputPath, HTTPRequestEntry request)
            throws RequestException {
        CurlBuilder curlBuilder = new CurlBuilder();
        HttpURLConnection connection = createConnection(settings, requestInputPath, request, curlBuilder);
        List<Path> sentFilePaths = null;
        Path bodyPath = null;

        if(isNotEmpty(request.getRequestFiles())) {
            addFileHeaders(connection, settings, request, curlBuilder);
            addFileMethod(connection, request, curlBuilder);
            sentFilePaths = sendFiles(connection, settings, request, curlBuilder);
        } else {
            addHeaders(connection, settings, request, curlBuilder);
            addMethod(connection, request, curlBuilder);
            bodyPath = sendBody(connection, settings, requestInputPath, request, curlBuilder);
        }

        HTTPResponseEntry response = buildResponse(connection, settings, requestInputPath, request, curlBuilder);
        response.setBodyPath(bodyPath);
        response.setSentFilePaths(sentFilePaths);

        return response;
    }

    private HttpURLConnection createConnection(Settings settings,
            String requestInputPath, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        URLBuilder urlBuilder = new URLBuilder()
            .setUrl(request.getUrl())
            .addAllQueryParams(request.getQueryParams());
        String generatedUrl = null;

        try {
            generatedUrl = urlBuilder.build();
            URL url = new URL(generatedUrl);

            curlBuilder.setUrl(url);

            log(settings, identifyMethod(request) + " " + url, null);

            return createConnection(settings, requestInputPath, request, url);
        } catch(IOException ex) {
            throw new RequestException(request, "Unable to create HTTP connection [" + generatedUrl + "] - " + ex.getMessage());
        }
    }

    private HttpURLConnection createConnection(Settings settings, String requestInputPath, HTTPRequestEntry request, URL url)
            throws IOException {
        RequestBehaviour behaviour = settings.getRequestBehaviour();

        switch(behaviour) {
            case CURL_ONLY:
            case PRINT_ONLY:
                return new HTTPMockConnection(url, settings, requestInputPath,
                    request.getName(), request.getDefaults(), null);
            default:
                LOGGER.fine("cURL/Print disabled");
        }

        if(settings.isMockRequest()) {
            return new HTTPMockConnection(url, settings, requestInputPath,
                request.getName(), request.getDefaults(), request.getMockDefinition());
        } else {
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
        List<HeaderAuthenticator<HTTPRequestEntry, ?>> headerAuthenticators = findAll(HeaderAuthenticator.class, name());

        headerAuthenticators.stream()
            .map(auth -> auth.getAuthHeaders(settings, request))
            .flatMap(List::stream)
            .forEach(header -> {
                connection.setRequestProperty(header.getLeft(), header.getRight());
                curlBuilder.addHeader(header.getLeft(), header.getRight());
                printableHeaders.append(header.getLeft())
                    .append(": ")
                    .append(header.getRight())
                    .append("\n");
            });

        request.getHeaders()
            .entrySet()
            .stream()
            .filter(e -> isNotBlank(e.getKey()))
            .forEach(e -> {
                connection.setRequestProperty(e.getKey(), e.getValue());
                curlBuilder.addHeader(e.getKey(), e.getValue());
                printableHeaders.append(e.getKey()).append(": ").append(e.getValue()).append("\n");
            });

        log(settings, printableHeaders.toString(), null);
    }

    private Path sendBody(HttpURLConnection connection, Settings settings, 
            String requestInputPath, HTTPRequestEntry request, CurlBuilder culrBuilder) {
        if(!connection.getDoOutput()) {
            return null;
        }

        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream())) {

            if(isNotBlank(request.getBodyContent())) {
                return sendBodyContent(dataOutputStream, settings, requestInputPath, request, culrBuilder);
            }

            if(isNotBlank(request.getBodyFilePath())) {
                return sendBodyFile(dataOutputStream, settings, requestInputPath, request, culrBuilder);
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
            String requestInputPath, HTTPRequestEntry request, CurlBuilder culrBuilder) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(requestInputPath)
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
            String requestInputPath, HTTPRequestEntry request, CurlBuilder culrBuilder) throws IOException {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(requestInputPath)
            .setRequestName(request.getName())
            .setExtension("body");
        Path temporalBodyPath = pathBuilder.buildCommandPath();
        Path requestBodyPath = Paths.get(request.getBodyFilePath());

        expandFile(settings, requestBodyPath, temporalBodyPath, request.getDefaults(),
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

    private List<Path> sendFiles(HttpURLConnection connection, Settings settings,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        Map<String, String> formData = isNotEmpty(request.getFormData()) ? request.getFormData() : new HashMap<>();
        String boundary = settings.get(REQUEST_FILE_BOUNDARY);
        StringBuilder printableFile = new StringBuilder();
        List<Path> sentFiles = new ArrayList<>();

        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream())) {

            for(HTTPRequestFileEntry requestFile : request.getRequestFiles()) {
                String[] pathParts = requestFile.getPath().split("/");
                String filename = isNotBlank(requestFile.getName())
                    ? requestFile.getName()
                    : pathParts[pathParts.length - 1];
                Path sentFile = appendFileToRequest(requestFile, dataOutputStream, filename, boundary);

                curlBuilder.addFile(requestFile.getField(), requestFile.getPath(), filename, requestFile.getMineType());

                printableFile.append(requestFile.getField()).append("=").append(requestFile.getPath()).append("\n");
                printableFile.append("filename=").append(requestFile.getPath()).append("\n");

                if(isNotBlank(requestFile.getMineType())) {
                    printableFile.append("type=").append(requestFile.getMineType()).append("\n");
                }

                sentFiles.add(sentFile);
            }

            for(Map.Entry<String, String> formEntry : formData.entrySet()) {
                dataOutputStream.writeBytes(MULTIPART_TWO_HYPHENS + boundary + MULTIPART_LINE_END);
                dataOutputStream.writeBytes("Content-Disposition: form-data; name=\"" + formEntry.getKey() + "\"" + MULTIPART_LINE_END);
                dataOutputStream.writeBytes("Content-Type: text/plain" + MULTIPART_LINE_END);
                dataOutputStream.writeBytes(MULTIPART_LINE_END);
                dataOutputStream.writeBytes(formEntry.getValue());
                dataOutputStream.writeBytes(MULTIPART_LINE_END);

                curlBuilder.addForm(formEntry.getKey(), formEntry.getValue());
                printableFile
                    .append(formEntry.getKey())
                    .append("=")
                    .append(formEntry.getValue())
                    .append("\n");
            };

            dataOutputStream.writeBytes(MULTIPART_TWO_HYPHENS + boundary + MULTIPART_TWO_HYPHENS + MULTIPART_LINE_END);

            log(settings, printableFile.toString(), null);

            return sentFiles;
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request files", ex);
        }
    }

    private Path appendFileToRequest(HTTPRequestFileEntry requestFile, DataOutputStream dataOutputStream,
            String filename, String boundary) {
        try(FileInputStream fileInputStream = new FileInputStream(requestFile.getPath())) {

            dataOutputStream.writeBytes(MULTIPART_TWO_HYPHENS + boundary + MULTIPART_LINE_END);
            dataOutputStream.writeBytes("Content-Disposition: form-data; name=\"" + requestFile.getField()
                + "\"; filename=\"" + filename + "\"" + MULTIPART_LINE_END);
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
            String requestInputPath, HTTPRequestEntry request, String filename) {
        String extension = isBlank(filename) ? "response" : null;
        Path responsePath = getOutputLocation(settings, filename, extension);

        if(responsePath == null) {
            responsePath = new OutputPathBuilder(settings)
                .setRequestPath(requestInputPath)
                .setRequestName(request.getName())
                .setFilename(filename)
                .setExtension(extension)
                .buildCommandPath();
        }

        if(readOutput(true, connection, responsePath)) {
            return responsePath;
        } else if(readOutput(false, connection, responsePath)) {
            return responsePath;
        } else {
            return null;
        }
    }

    private Path getOutputLocation(Settings settings, String filename, String extension) {
        if(!settings.isSaveOutputInLocation()) {
            return null;
        }

        AtomicReference<String> location = new AtomicReference<>();
        exec(settings, location::set, true, FINE, "echo " + settings.getDownloadsLocation());

        if(isBlank(location.get())) {
            LOGGER.fine("'downloadsLocation' is null or empty");
            return null;
        } else {
            Path downloadsLocation = Paths.get(strip(location.get(), "\""));
            return new OutputPathBuilder(settings)
                .setFilename(filename)
                .setExtension(extension)
                .buildFilePath(downloadsLocation, null);
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
            Settings settings, String requestInputPath, HTTPRequestEntry request,
            CurlBuilder curlBuilder) {
        HTTPResponseEntry response = new HTTPResponseEntry();
        StringBuilder printableResponse = new StringBuilder();

        try {
            String headerResultKey = "http.header.result." + UUID.randomUUID();
            response.setRequestUrl(connection.getURL().toString());
            response.setCurlCommand(curlBuilder.build());
            response.setStatusCode(connection.getResponseCode());
            response.setResponseHeaders(connection.getHeaderFields()
                .entrySet()
                .stream()
                .collect(Collectors.toMap(
                    e -> getOrDefault(e.getKey(), headerResultKey),
                    e -> String.join(",", e.getValue())
                ))
            );
            response.setResult(response.getResponseHeaders().remove(headerResultKey));

            String filename = getOutputFilename(response);
            Path responsePath = readOutput(connection, settings, requestInputPath, request, filename);

            response.setResponsePath(responsePath);

            if(isNotBlank(response.getResult())) {
                printableResponse.append(response.getResult()).append("\n");
            }

            response.getResponseHeaders()
                .forEach((header, value) ->
                    printableResponse.append(header).append(": ").append(value).append("\n"));

            log(settings, printableResponse.toString(), null);

            return response;
        } catch(IOException ex) {
            throw new IllegalStateException("Error on creating response", ex);
        }
    }

    private String identifyMethod(HTTPRequestEntry request) {
        if(isNotBlank(request.getMethod())) {
            return request.getMethod();
        }
        if(isNotEmpty(request.getRequestFiles())) {
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
