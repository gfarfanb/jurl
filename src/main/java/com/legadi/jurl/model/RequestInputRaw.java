package com.legadi.jurl.model;

import java.util.Map;

import com.google.gson.annotations.Expose;
import com.google.gson.annotations.JsonAdapter;
import com.legadi.jurl.model.adapter.APIDeserializer;
import com.legadi.jurl.model.adapter.FlowsDeserializer;
import com.legadi.jurl.model.adapter.RequestsDeserializer;

public class RequestInputRaw {

    @Expose(serialize = false, deserialize = false)
    private String path;
    private Map<String, Map<String, String>> configs;
    private String defaultRequest;
    private String defaultFlow;
    @JsonAdapter(APIDeserializer.class)
    private String api;
    @JsonAdapter(RequestsDeserializer.class)
    private Map<String, String> requests;
    @JsonAdapter(FlowsDeserializer.class)
    private Map<String, String[]> flows;

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

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

    public String getApi() {
        return api;
    }

    public void setApi(String api) {
        this.api = api;
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
