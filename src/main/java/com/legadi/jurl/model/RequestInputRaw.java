package com.legadi.jurl.model;

import java.util.Map;

public class RequestInputRaw {

    private Map<String, Map<String, String>> configs;
    private String defaultRequest;
    private String defaultFlow;
    private Map<String, String> requests;
    private Map<String, String[]> flows;

    public Map<String, Map<String, String>> getConfigs() {
        return configs;
    }

    public void setConfigs(Map<String, Map<String, String>> configs) {
        this.configs = configs;
    }

    public String getDefaultRequest() {
        return defaultRequest;
    }

    public void setDefaultRequest(String defaultRequest) {
        this.defaultRequest = defaultRequest;
    }

    public String getDefaultFlow() {
        return defaultFlow;
    }

    public void setDefaultFlow(String defaultFlow) {
        this.defaultFlow = defaultFlow;
    }

    public Map<String, String> getRequests() {
        return requests;
    }

    public void setRequests(Map<String, String> requests) {
        this.requests = requests;
    }

    public Map<String, String[]> getFlows() {
        return flows;
    }

    public void setFlows(Map<String, String[]> flows) {
        this.flows = flows;
    }
}
