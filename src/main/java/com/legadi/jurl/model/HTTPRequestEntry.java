package com.legadi.jurl.model;

import java.nio.charset.Charset;
import java.util.Map;

public final class HTTPRequestEntry extends RequestEntry {

    private String method;
    private Charset bodyCharset;
    private String bodyContent;
    private String bodyFile;
    private String requestFile;
    private Map<String, String> headers;
    private Map<String, String> queryParams;

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

    public String getBodyFile() {
        return bodyFile;
    }

    public void setBodyFile(String bodyFile) {
        this.bodyFile = bodyFile;
    }

    public String getRequestFile() {
        return requestFile;
    }

    public void setRequestFile(String requestFile) {
        this.requestFile = requestFile;
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
}
