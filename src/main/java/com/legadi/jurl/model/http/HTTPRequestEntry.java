package com.legadi.jurl.model.http;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legadi.jurl.model.AssertionEntry;
import com.legadi.jurl.model.RequestEntry;

public class HTTPRequestEntry extends RequestEntry {

    private String method;
    private Map<String, String> queryParams = new HashMap<>();
    private Map<String, String> headers = new HashMap<>();
    private String bodyCharset = StandardCharsets.UTF_8.name();
    private String bodyContent;
    private String bodyFilePath;
    private HTTPRequestFileEntry requestFile;
    private Map<String, String> outputMappings = new HashMap<>();
    private List<AssertionEntry> assertions = new LinkedList<>();

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

    public Map<String, String> getOutputMappings() {
        return outputMappings;
    }

    public void setOutputMappings(Map<String, String> outputMappings) {
        this.outputMappings = outputMappings;
    }

    public List<AssertionEntry> getAssertions() {
        return assertions;
    }

    public void setAssertions(List<AssertionEntry> assertions) {
        this.assertions = assertions;
    }
}
