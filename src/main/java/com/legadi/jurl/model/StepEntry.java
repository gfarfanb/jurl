package com.legadi.jurl.model;

import java.util.Map;

import com.google.gson.annotations.Expose;

public class StepEntry {

    @Expose(serialize = false, deserialize = false)
    private String flowName;
    private String requestInputPath;
    private Map<String, String[]> options;

    public String getFlowName() {
        return flowName;
    }

    public void setFlowName(String flowName) {
        this.flowName = flowName;
    }

    public String getRequestInputPath() {
        return requestInputPath;
    }

    public void setRequestInputPath(String requestInputPath) {
        this.requestInputPath = requestInputPath;
    }

    public Map<String, String[]> getOptions() {
        return options;
    }

    public void setOptions(Map<String, String[]> options) {
        this.options = options;
    }
}
