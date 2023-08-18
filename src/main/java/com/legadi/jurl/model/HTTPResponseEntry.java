package com.legadi.jurl.model;

import java.nio.file.Path;
import java.util.Map;

public class HTTPResponseEntry extends ResponseEntry {

    private Path responsePath;
    private int statusCode;
    private Map<String, String> responseHeaders;

    public Path getResponsePath() {
        return responsePath;
    }

    public void setResponsePath(Path responsePath) {
        this.responsePath = responsePath;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public Map<String, String> getResponseHeaders() {
        return responseHeaders;
    }

    public void setResponseHeaders(Map<String, String> responseHeaders) {
        this.responseHeaders = responseHeaders;
    }
}
