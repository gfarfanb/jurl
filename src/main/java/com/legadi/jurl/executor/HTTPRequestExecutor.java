package com.legadi.jurl.executor;

import com.legadi.jurl.common.CurlBuilder;
import com.legadi.jurl.common.ExecutionTag;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.WriteOnOutputStreamException;
import com.legadi.jurl.model.HTTPRequestEntry;
import com.legadi.jurl.model.HTTPRequestFileEntry;
import com.legadi.jurl.model.HTTPResponseEntry;
import com.legadi.jurl.model.RequestEntry;

import static com.legadi.jurl.common.RequestUtils.MULTIPART_LINE_END;
import static com.legadi.jurl.common.RequestUtils.MULTIPART_MAX_BUFFER_SIZE;
import static com.legadi.jurl.common.RequestUtils.MULTIPART_TWO_HYPHENS;
import static com.legadi.jurl.common.RequestUtils.isHTTP;
import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;
import static com.legadi.jurl.common.StringUtils.stripEnd;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HTTPRequestExecutor implements RequestExecutor<HTTPRequestEntry, HTTPResponseEntry> {

    private Logger logger = LoggerFactory.getLogger(HTTPRequestExecutor.class);

    private Settings settings = new Settings();

    @Override
    public boolean accepts(RequestEntry request) {
        return isHTTP(request);
    }

    @Override
    public HTTPResponseEntry executeRequest(ExecutionTag executionTag, HTTPRequestEntry request) throws RequestException {
        HttpURLConnection connection = createConnection(request);
        CurlBuilder curlBuilder = new CurlBuilder(connection);

        addHeaders(connection, request, curlBuilder);

        if(request.getRequestFile() != null) {
            sendFile(connection, request, curlBuilder);
        } else {
            addMethod(connection, request, curlBuilder);
            sendBody(connection, request, curlBuilder);
        }

        Path responsePath = readOutput(executionTag, connection);

        return buildResponse(responsePath, connection, request, curlBuilder);
    }

    private HttpURLConnection createConnection(HTTPRequestEntry request) {
        if(isBlank(request.getUrl())) {
            throw new RequestException(request, "HTTP resource not defined");
        }

        String urlBase = stripEnd(request.getUrl().toString(), "?&");
        StringBuilder urlPart = new StringBuilder(urlBase);

        if(urlBase.contains("?")) {
            request.getQueryParams().entrySet()
                .forEach(param -> urlPart
                    .append('&')
                    .append(param.getKey())
                    .append('=')
                    .append(param.getValue()));
        } else {
            AtomicBoolean first = new AtomicBoolean(true);
            request.getQueryParams().entrySet()
                .forEach(param -> {
                    if(first.get()) {
                        first.set(false);
                        urlPart
                            .append(param.getKey())
                            .append('=')
                            .append(param.getValue());
                    } else {
                        urlPart
                            .append('&')
                            .append(param.getKey())
                            .append('=')
                            .append(param.getValue());
                    }
                });
        }

        try {
            URL url = new URL(urlPart.toString());
            return (HttpURLConnection) url.openConnection();
        } catch(MalformedURLException ex) {
            throw new RequestException(request, "Malformed HTTP resource: " + urlPart);
        } catch(IOException ex) {
            throw new RequestException(request, "Unable to create HTTP connection: " + urlPart);
        }
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

    private void addHeaders(HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        if(request.getHeaders() == null) {
            return;
        }

        request.getHeaders().forEach(
            (header, value) -> {
                connection.setRequestProperty(header, value);
                curlBuilder.addHeader(header, value);
            }
        );
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

            throw new IllegalArgumentException("request.bodyContent or request.bodyFile is null or empty");
        } catch(IOException ex) {
            throw new RequestException(request, "Unable to create output stream for request body");
        } catch(IllegalArgumentException | WriteOnOutputStreamException ex) {
            throw new RequestException(request, ex.getMessage());
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
            logger.error("Unable to create output stream for request file: " + requestFile.getPath(), ex);
            throw new RequestException(request, "Unable to create output stream for request file: " + requestFile.getPath());
        }
    }

    private Path readOutput(ExecutionTag executionTag, HttpURLConnection connection) {
        Path outputPath = Paths.get(settings.get("application.execution.output.path"));
        File responseFile = outputPath.resolve(executionTag + ".response").toFile();

        try(BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                FileWriter responseWriter = new FileWriter(responseFile)) {

            String line;
            while((line = br.readLine()) != null) {
                responseWriter.write(line);
            }

            return responseFile.toPath();
        } catch(IOException ex) {
            logger.debug("Error on reading response", ex);
            return null;
        }
    }

    private HTTPResponseEntry buildResponse(Path responsePath, HttpURLConnection connection, HTTPRequestEntry request, CurlBuilder curlBuilder) {
        HTTPResponseEntry response = new HTTPResponseEntry();

        try {
            response.setCurl(curlBuilder.build());
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
            logger.error("Error on creating response", ex);
            throw new RequestException(request, "Error on creating response");
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

    private void writeLine(DataOutputStream dataOutputStream, String line, Charset charset) throws WriteOnOutputStreamException {
        try {
            byte[] input = line.getBytes(charset);
            dataOutputStream.write(input, 0, input.length);
        } catch(IOException ex) {
            logger.error("Unable to write line", ex);
            throw new WriteOnOutputStreamException("Unable to write line - " + ex.getMessage());
        }
    }
}
