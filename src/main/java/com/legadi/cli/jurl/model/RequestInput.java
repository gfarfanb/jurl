package com.legadi.cli.jurl.model;

import java.util.HashMap;
import java.util.Map;

public class RequestInput<T extends RequestEntry<? extends MockEntry>> {

    private String defaultRequest;
    private T api;
    private Map<String, T> requests = new HashMap<>();
    private Map<String, FlowEntry> flows = new HashMap<>();

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

    public Map<String, T> getRequests() {
        return requests;
    }

    public void setRequests(Map<String, T> requests) {
        this.requests = requests;
    }

    public Map<String, FlowEntry> getFlows() {
        return flows;
    }

    public void setFlows(Map<String, FlowEntry> flows) {
        this.flows = flows;
    }
}
