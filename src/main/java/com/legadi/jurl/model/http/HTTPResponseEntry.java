package com.legadi.jurl.model.http;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import com.legadi.jurl.model.ResponseEntry;

public class HTTPResponseEntry extends ResponseEntry {

    private Path bodyPath;
    private Path sentFilePath;
    private Path responsePath;
    private int statusCode;
    private Map<String, String> responseHeaders = new HashMap<>();

    public Path getBodyPath() {
        return bodyPath;
    }

    public void setBodyPath(Path bodyPath) {
        this.bodyPath = bodyPath;
    }

    public Path getSentFilePath() {
        return sentFilePath;
    }

    public void setSentFilePath(Path sentFilePath) {
        this.sentFilePath = sentFilePath;
    }

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
