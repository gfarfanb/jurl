package com.legadi.jurl.model;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Map;

public final class HTTPRequestEntry extends RequestEntry {

    private String method;
    private Charset bodyCharset = StandardCharsets.UTF_8;
    private String bodyContent;
    private String bodyFilePath;
    private Map<String, String> headers;
    private Map<String, String> queryParams;
    private HTTPRequestFileEntry requestFile;

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public Charset getBodyCharset() {
        return bodyCharset;
    }

    public void setBodyCharset(Charset bodyCharset) {
        this.bodyCharset = bodyCharset;
    }

    public String getBodyContent() {
        return bodyContent;
    }

    public void setBodyContent(String bodyContent) {
        this.bodyContent = bodyContent;
    }

    public String getBodyFilePath() {
        return bodyFilePath;
    }

    public void setBodyFilePath(String bodyFilePath) {
        this.bodyFilePath = bodyFilePath;
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public Map<String, String> getQueryParams() {
        return queryParams;
    }

    public void setQueryParams(Map<String, String> queryParams) {
        this.queryParams = queryParams;
    }

    public HTTPRequestFileEntry getRequestFile() {
        return requestFile;
    }

    public void setRequestFile(HTTPRequestFileEntry requestFile) {
        this.requestFile = requestFile;
    }
}
