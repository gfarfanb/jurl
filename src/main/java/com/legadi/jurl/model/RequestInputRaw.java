package com.legadi.jurl.model;

import java.util.List;

public class RequestInputRaw extends ConfigurableRequest {

    private String request;
    private List<String> steps;

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
