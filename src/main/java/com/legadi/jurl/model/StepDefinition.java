package com.legadi.jurl.model;

import java.util.Map;

public class StepDefinition {

    private String requestInputPath;
    private Map<String, String[]> options;

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
