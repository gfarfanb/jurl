package com.legadi.cli.jurl.model.http;

import java.util.HashMap;
import java.util.Map;

import com.legadi.cli.jurl.model.MockEntry;

public class HTTPMockEntry extends MockEntry {

    private Map<String, String> responseHeaders = new HashMap<>();
    private String responseContent;
    private String responseFilePath;
    private String exceptionClassOnOutputStream;
    private String exceptionClassOnResponseCode;

    public Map<String, String> getResponseHeaders() {
        return responseHeaders;
    }

    public void setResponseHeaders(Map<String, String> responseHeaders) {
        this.responseHeaders = responseHeaders;
    }

    public String getResponseContent() {
        return responseContent;
    }

    public void setResponseContent(String responseContent) {
        this.responseContent = responseContent;
    }

    public String getResponseFilePath() {
        return responseFilePath;
    }

    public void setResponseFilePath(String responseFilePath) {
        this.responseFilePath = responseFilePath;
    }

    public String getExceptionClassOnOutputStream() {
        return exceptionClassOnOutputStream;
    }

    public void setExceptionClassOnOutputStream(String exceptionClassOnOutputStream) {
        this.exceptionClassOnOutputStream = exceptionClassOnOutputStream;
    }

    public String getExceptionClassOnResponseCode() {
        return exceptionClassOnResponseCode;
    }

    public void setExceptionClassOnResponseCode(String exceptionClassOnResponseCode) {
        this.exceptionClassOnResponseCode = exceptionClassOnResponseCode;
    }
}
