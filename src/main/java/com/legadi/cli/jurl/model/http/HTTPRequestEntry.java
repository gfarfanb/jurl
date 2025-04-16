package com.legadi.cli.jurl.model.http;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.legadi.cli.jurl.model.RequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPRequestEntry extends RequestEntry<HTTPMockEntry> {

    private String method;
    private Map<String, String> queryParams = new HashMap<>();
    private Map<String, String> headers = new HashMap<>();
    private String bodyCharset;
    private String bodyContent;
    private String bodyFilePath;
    private List<HTTPRequestFileEntry> requestFiles = new ArrayList<>();
    private Map<String, String> formData = new HashMap<>();
    private HTTPBasicAuthEntry basicAuth;
    private HTTPTokenAuthEntry tokenAuth;

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

    public List<HTTPRequestFileEntry> getRequestFiles() {
        return requestFiles;
    }

    public void setRequestFiles(List<HTTPRequestFileEntry> requestFiles) {
        this.requestFiles = requestFiles;
    }

    public Map<String, String> getFormData() {
        return formData;
    }

    public void setFormData(Map<String, String> formData) {
        this.formData = formData;
    }

    public HTTPBasicAuthEntry getBasicAuth() {
        return basicAuth;
    }

    public void setBasicAuth(HTTPBasicAuthEntry basicAuth) {
        this.basicAuth = basicAuth;
    }

    public HTTPTokenAuthEntry getTokenAuth() {
        return tokenAuth;
    }

    public void setTokenAuth(HTTPTokenAuthEntry tokenAuth) {
        this.tokenAuth = tokenAuth;
    }
}
