package com.legadi.cli.jurl.common;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

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

    public void removeAll(String environment, Set<String> properties) {
        getResource(environment).keySet().removeAll(properties);
    }

    public void removeAllInCommon(Set<String> properties) {
        commonData.keySet().removeAll(properties);
    }

    private Map<String, T> getResource(String environment) {
        if(data.get(environment) == null) {
            data.put(environment, new HashMap<>());
        }
        return data.get(environment);
    }
}
