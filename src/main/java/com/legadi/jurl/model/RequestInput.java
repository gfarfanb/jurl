package com.legadi.jurl.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RequestInput<T extends RequestEntry<? extends MockEntry>> {

    private String defaultRequest;
    private T api;
    private Map<String, String> config = new HashMap<>();
    private Map<String, T> requests = new HashMap<>();
    private Map<String, List<StepEntry>> flows = new HashMap<>();

    public String getDefaultRequest() {
        return defaultRequest;
    }

    public void setDefaultRequest(String defaultRequest) {
        this.defaultRequest = defaultRequest;
    }

    public T getApi() {
        return api;
    }

    public void setApi(T api) {
        this.api = api;
    }

    public Map<String, String> getConfig() {
        return config;
    }

    public void setConfig(Map<String, String> config) {
        this.config = config;
    }

    public Map<String, T> getRequests() {
        return requests;
    }

    public void setRequests(Map<String, T> requests) {
        this.requests = requests;
    }

    public Map<String, List<StepEntry>> getFlows() {
        return flows;
    }

    public void setFlows(Map<String, List<StepEntry>> flows) {
        this.flows = flows;
    }
}
