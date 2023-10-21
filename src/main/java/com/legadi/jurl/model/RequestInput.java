package com.legadi.jurl.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RequestInput<T extends RequestEntry<? extends MockEntry>> {

    private String defaultRequest;
    private String defaultFlow;
    private T api;
    private Map<String, T> requests = new HashMap<>();
    private Map<String, List<StepEntry>> flows = new HashMap<>();

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

    public T getApi() {
        return api;
    }

    public void setApi(T api) {
        this.api = api;
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
