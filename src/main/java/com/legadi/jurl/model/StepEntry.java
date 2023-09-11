package com.legadi.jurl.model;

import java.util.Map;

import com.google.gson.annotations.Expose;

public class StepEntry {

    @Expose(serialize = false, deserialize = false)
    private String name;
    private String requestInputName;
    private String requestInputPath;
    private Map<String, String[]> options;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getRequestInputName() {
        return requestInputName;
    }

    public void setRequestInputName(String requestInputName) {
        this.requestInputName = requestInputName;
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
