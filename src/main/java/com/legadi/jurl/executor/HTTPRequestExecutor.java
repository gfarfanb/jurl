package com.legadi.jurl.executor;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.CurlBuilder;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.common.URLBuilder;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.Credential;
import com.legadi.jurl.model.HTTPRequestEntry;
import com.legadi.jurl.model.HTTPRequestFileEntry;
import com.legadi.jurl.model.HTTPResponseEntry;
import com.legadi.jurl.model.RequestEntry;

import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
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
import java.util.stream.Collectors;

import static java.util.logging.Level.FINE;

public class HTTPRequestExecutor implements RequestExecutor<HTTPRequestEntry, HTTPResponseEntry> {

    private static final Logger LOGGER = Logger.getLogger(HTTPRequestExecutor.class.getName());

    private static final String MULTIPART_LINE_END = "\r\n";
    private static final String MULTIPART_TWO_HYPHENS = "--";
    private static final int MULTIPART_MAX_BUFFER_SIZE = 1 * 1024 * 1024;

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
            sendFile(connection, request, curlBuilder);
        } else {
            addMethod(connection, request, curlBuilder);
            sendBody(connection, request, curlBuilder);
        }

        Path responsePath = readOutput(settings, connection);

        return buildResponse(responsePath, connection, request, curlBuilder);
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
        if(request.getHeaders() == null) {
            return;
        }

        request.getHeaders().forEach(
            (header, value) -> {
                connection.setRequestProperty(header, value);
                curlBuilder.addHeader(header, value);
            }
        );

        switch(settings.getAuthorizationType()) {
            case BASIC:
                Credential basicCredential = settings.getCredential();
                String basicDecoded = basicCredential.getUsername() + ":" + basicCredential.getPassword();
                String basicValue = Base64.getEncoder().encodeToString(basicDecoded.getBytes());
                connection.setRequestProperty("Authorization", "Basic " + basicValue);
                curlBuilder.addHeader("Authorization", "Basic " + basicValue);
                break;
            case TOKEN:
                Credential tokenCredential = settings.getCredential();
                connection.setRequestProperty("Authorization", "Bearer " + tokenCredential.getToken());
                curlBuilder.addHeader("Authorization", "Bearer " + tokenCredential.getToken());
                break;
            default:
                LOGGER.fine("Authorization type not specified");
                break;
        }
    }

    private void sendBody(HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder culrBuilder) {
        try(DataOutputStream dataOutputStream = new DataOutputStream(connection.getOutputStream())) {

            if(isNotBlank(request.getBodyContent())) {
                writeLine(dataOutputStream, request.getBodyContent(), request.getBodyCharset());
                culrBuilder.setData(request.getBodyContent());
                return;
            }

            if(isNotBlank(request.getBodyFilePath())) {
                Path bodyPath = Paths.get(request.getBodyFilePath());

                try(BufferedReader br = Files.newBufferedReader(bodyPath)) {
                    br.lines().forEach(line -> writeLine(dataOutputStream, line, request.getBodyCharset()));
                }

                culrBuilder.setDataBinary(request.getBodyFilePath());
                return;
            }

            throw new RequestException(request, "request.bodyContent or request.bodyFile is null or empty");
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request body", ex);
        }
    }

    private void sendFile(HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        HTTPRequestFileEntry requestFile = validateAndGetRequestFile(request);
        String boundary = "*****" + Long.toString(System.currentTimeMillis()) + "*****";
        String[] pathParts = requestFile.getPath().split("/");
        String filename = isNotBlank(requestFile.getName()) ? requestFile.getName() : pathParts[pathParts.length - 1];
        Map<String, String> formData = requestFile.getFormData() != null ? requestFile.getFormData() : new HashMap<>();

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
            formData.forEach((field, value) -> curlBuilder.addForm(field, value));
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create output stream for request file: " + requestFile.getPath(), ex);
        }
    }

    private Path readOutput(Settings settings, HttpURLConnection connection) {
        File responseFile = settings.getOutputPath().resolve(settings.getExecutionTag() + ".response").toFile();

        try(BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                FileWriter responseWriter = new FileWriter(responseFile)) {

            String line;
            while((line = br.readLine()) != null) {
                responseWriter.write(line);
            }

            return responseFile.toPath();
        } catch(IOException ex) {
            LOGGER.log(FINE, "Error on reading response", ex);
            return null;
        }
    }

    private HTTPResponseEntry buildResponse(Path responsePath, HttpURLConnection connection,
            HTTPRequestEntry request, CurlBuilder curlBuilder) {
        HTTPResponseEntry response = new HTTPResponseEntry();

        try {
            response.setRequestUrl(connection.getURL().toString());
            response.setCurlCommand(curlBuilder.build());
            response.setResponsePath(responsePath);
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
