package com.legadi.jurl.model;

import java.nio.charset.StandardCharsets;
import java.util.Map;

public class HTTPRequestEntry extends RequestEntry {

    private String method;
    private Map<String, String> queryParams;
    private Map<String, String> headers;
    private String bodyCharset = StandardCharsets.UTF_8.name();
    private String bodyContent;
    private String bodyFilePath;
    private HTTPRequestFileEntry requestFile;
    private AuthorizationType authType;
    private String authCredentialId;

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public Map<String, String> getQueryParams() {
        return queryParams;
    }

    public void setQueryParams(Map<String, String> queryParams) {
        this.queryParams = queryParams;
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public String getBodyCharset() {
        return bodyCharset;
    }

    public void setBodyCharset(String bodyCharset) {
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

    public HTTPRequestFileEntry getRequestFile() {
        return requestFile;
    }

    public void setRequestFile(HTTPRequestFileEntry requestFile) {
        this.requestFile = requestFile;
    }

    public AuthorizationType getAuthType() {
        return authType;
    }

    public void setAuthType(AuthorizationType authType) {
        this.authType = authType;
    }

    public String getAuthCredentialId() {
        return authCredentialId;
    }

    public void setAuthCredentialId(String authCredentialId) {
        this.authCredentialId = authCredentialId;
    }
}
