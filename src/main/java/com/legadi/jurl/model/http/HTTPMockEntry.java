package com.legadi.jurl.model.http;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.legadi.jurl.model.MockEntry;

public class HTTPMockEntry extends MockEntry {

    private Map<String, List<String>> responseHeaders = new HashMap<>();
    private String responseContent;
    private String responseFilePath;

    public Map<String, List<String>> getResponseHeaders() {
        return responseHeaders;
    }

    public void setResponseHeaders(Map<String, List<String>> responseHeaders) {
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
}
