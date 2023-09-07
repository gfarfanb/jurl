package com.legadi.jurl.model;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

public class HTTPRequestEntry extends RequestEntry {

    private String method;
    private Map<String, String> queryParams;
    private Map<String, String> headers;
    private String bodyCharset = StandardCharsets.UTF_8.name();
    private String bodyContent;
    private String bodyFilePath;
    private HTTPRequestFileEntry requestFile;
    private OutputType outputType = OutputType.JSON;
    private Map<String, String> outputMappings;
    private List<AssertEntry> asserts;

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

    public OutputType getOutputType() {
        return outputType;
    }

    public void setOutputType(OutputType outputType) {
        this.outputType = outputType;
    }

    public Map<String, String> getOutputMappings() {
        return outputMappings;
    }

    public void setOutputMappings(Map<String, String> outputMappings) {
        this.outputMappings = outputMappings;
    }

    public List<AssertEntry> getAsserts() {
        return asserts;
    }

    public void setAsserts(List<AssertEntry> asserts) {
        this.asserts = asserts;
    }
}
