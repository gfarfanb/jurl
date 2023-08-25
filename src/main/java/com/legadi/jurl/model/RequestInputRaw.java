package com.legadi.jurl.model;

import java.util.List;
import java.util.Map;

public class RequestInputRaw {

    private Map<String, String> config;
    private String request;
    private List<String> steps;

    public Map<String, String> getConfig() {
        return config;
    }

    public void setConfig(Map<String, String> config) {
        this.config = config;
    }

    public String getRequest() {
        return request;
    }

    public void setRequest(String request) {
        this.request = request;
    }

    public List<String> getSteps() {
        return steps;
    }

    public void setSteps(List<String> steps) {
        this.steps = steps;
    }
}
