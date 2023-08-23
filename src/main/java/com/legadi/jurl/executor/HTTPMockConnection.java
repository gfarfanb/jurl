package com.legadi.jurl.executor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import static com.legadi.jurl.common.StringUtils.isNotBlank;
import static com.legadi.jurl.common.StringUtils.toJsonString;

public class HTTPMockConnection extends HttpURLConnection {

    private static final Logger LOGGER = Logger.getLogger(HTTPMockConnection.class.getName());

    private String responseContent;
    private int responseCode;
    private Map<String, List<String>> responseHeaders;

    public HTTPMockConnection() {
        super(null);
    }

    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }

    public void setResponseCode(int responseCode) {
        this.responseCode = responseCode;
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
    public void setDoOutput(boolean output) {
        LOGGER.fine("[mock-connection] Calling setDoOutput(" + output + ")");
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
        } else {
            LOGGER.fine("[mock-connection] Calling getInputStream():ByteArrayInputStream");
            return new ByteArrayInputStream(new byte[0]);
        }
    }

    @Override
    public int getResponseCode() throws IOException {
        LOGGER.fine("[mock-connection] Calling getResponseCode():" + responseCode);
        return responseCode;
    }

    @Override
    public Map<String, List<String>> getHeaderFields() {
        if(responseHeaders != null) {
            LOGGER.fine("[mock-connection] Calling getHeaderFields():" + responseHeaders.getClass().getSimpleName()
                + "-" + toJsonString(responseHeaders));
            return responseHeaders;
        } else {
            LOGGER.fine("[mock-connection] Calling getHeaderFields():HashMap");
            return new HashMap<>();
        }
    }

    @Override
    public void setUseCaches(boolean usecaches) {
        LOGGER.fine("[mock-connection] Calling setUseCaches(" + usecaches + ")");
    }
}
