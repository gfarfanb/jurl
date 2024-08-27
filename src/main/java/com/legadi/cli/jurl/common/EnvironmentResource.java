package com.legadi.cli.jurl.common;

import java.util.HashMap;
import java.util.Map;

public class EnvironmentResource<T> {

    private final Map<String, Map<String, T>> data = new HashMap<>();
    private final Map<String, T> commonData = new HashMap<>();

    public T get(String environment, String key) {
        return getResource(environment)
            .getOrDefault(key, commonData.get(key));
    }

    public T getOrDefault(String environment, String key, T defaultValue) {
        return getResource(environment)
            .getOrDefault(key, commonData.getOrDefault(key, defaultValue));
    }

    public void putAll(String environment, Map<String, T> resources) {
        if(resources == null) {
            return;
        }
        getResource(environment).putAll(resources);
    }

    public void putAllInCommon(Map<String, T> resources) {
        if(resources == null) {
            return;
        }
        commonData.putAll(resources);
    }

    private Map<String, T> getResource(String environment) {
        if(!data.containsKey(environment)) {
            data.put(environment, new HashMap<>());
        }
        return data.get(environment);
    }
}
