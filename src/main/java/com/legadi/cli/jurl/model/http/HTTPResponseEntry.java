package com.legadi.cli.jurl.model.http;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.legadi.cli.jurl.model.ResponseEntry;

public class HTTPResponseEntry extends ResponseEntry {

    private Path bodyPath;
    private Path responsePath;
    private int statusCode;
    private List<Path> sentFilePaths;
    private Map<String, String> responseHeaders = new HashMap<>();

    public Path getBodyPath() {
        return bodyPath;
    }

    public void setBodyPath(Path bodyPath) {
        this.bodyPath = bodyPath;
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

    public List<Path> getSentFilePaths() {
        return sentFilePaths;
    }

    public void setSentFilePaths(List<Path> sentFilePaths) {
        this.sentFilePaths = sentFilePaths;
    }

    public Map<String, String> getResponseHeaders() {
        return responseHeaders;
    }

    public void setResponseHeaders(Map<String, String> responseHeaders) {
        this.responseHeaders = responseHeaders;
    }
}
