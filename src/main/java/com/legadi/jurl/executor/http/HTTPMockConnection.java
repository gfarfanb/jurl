package com.legadi.jurl.executor.http;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.JsonUtils.toJsonString;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import com.legadi.jurl.model.http.HTTPMockEntry;

public class HTTPMockConnection extends HttpURLConnection {

    private static final Logger LOGGER = Logger.getLogger(HTTPMockConnection.class.getName());

    private URL url;
    private int responseCode;
    private String responseContent;
    private String responseFilePath;
    private Map<String, List<String>> responseHeaders = new HashMap<>();
    private boolean doOutput;

    public HTTPMockConnection(URL url, HTTPMockEntry mockEntry) {
        super(null);

        this.url = url;

        if(mockEntry != null) {
            this.responseCode = mockEntry.getStatusCode();
            this.responseContent = mockEntry.getResponseContent();
            this.responseFilePath = mockEntry.getResponseFilePath();

            this.responseHeaders.putAll(mockEntry.getResponseHeaders());
            this.responseHeaders.put(null, Arrays.asList("HTTP/1.1 " + responseCode));
        }
    }

    @Override
    public URL getURL() {
        return url;
    }

    @Override
    public void disconnect() {
        LOGGER.fine("[mock-connection] Calling disconnect()");
    }

    @Override
    public boolean usingProxy() {
        LOGGER.fine("[mock-connection] Calling usingProxy():false");
        return false;
    }

    @Override
    public void connect() throws IOException {
        LOGGER.fine("[mock-connection] Calling connect()");
    }

    @Override
    public void setRequestMethod(String method) throws ProtocolException {
        LOGGER.fine("[mock-connection] Calling setRequestMethod(" + method + ")");
    }

    @Override
    public void setRequestProperty(String key, String value) {
        LOGGER.fine("[mock-connection] Calling setRequestProperty(" + key + ", " + value + ")");
    }

    @Override
    public void setDoOutput(boolean doOutput) {
        LOGGER.fine("[mock-connection] Calling setDoOutput(" + doOutput + ")");
        this.doOutput = doOutput;
    }

    @Override
    public boolean getDoOutput() {
        LOGGER.fine("[mock-connection] Calling getDoOutput():" + doOutput);
        return doOutput;
    }

    @Override
    public OutputStream getOutputStream() throws IOException {
        LOGGER.fine("[mock-connection] Calling getOutputStream():ByteArrayOutputStream");
        return new ByteArrayOutputStream();
    }

    @Override
    public void setDoInput(boolean input) {
        LOGGER.fine("[mock-connection] Calling setDoInput(" + input + ")");
    }

    @Override
    public InputStream getInputStream() throws IOException {
        if(isNotBlank(responseContent)) {
            LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream - " + responseContent);
            return new ByteArrayInputStream(responseContent.getBytes());
        }
        if(isNotBlank(responseFilePath)) {
            LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream - " + responseFilePath);
            return Files.newInputStream(Paths.get(responseFilePath));
        }

        LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream");
        return new ByteArrayInputStream(new byte[0]);
    }

    @Override
    public int getResponseCode() throws IOException {
        LOGGER.fine("[mock-connection] Calling getResponseCode():" + responseCode);
        return responseCode;
    }

    @Override
    public Map<String, List<String>> getHeaderFields() {
        if(isNotEmpty(responseHeaders)) {
            LOGGER.fine("[mock-connection] Calling getHeaderFields():" + responseHeaders.getClass().getSimpleName()
                + "-" + toJsonString(responseHeaders));
            return responseHeaders;
        }

        LOGGER.fine("[mock-connection] Calling getHeaderFields():HashMap");
        return new HashMap<>();
    }

    @Override
    public void setUseCaches(boolean usecaches) {
        LOGGER.fine("[mock-connection] Calling setUseCaches(" + usecaches + ")");
    }
}
