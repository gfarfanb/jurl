package com.legadi.jurl.model;

import java.util.Map;

public abstract class ConfigurableRequest {

    protected Map<String, String> config;

    public Map<String, String> getConfig() {
        return config;
    }

    public void setConfig(Map<String, String> config) {
        this.config = config;
    }
}
