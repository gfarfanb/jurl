package com.legadi.jurl.embedded.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.legadi.jurl.common.Pair;

public class RequestCatcher {

    private final Map<UUID, Map<String, Object>> data = Collections.synchronizedMap(new HashMap<>());
    private final Map<String, Pair<UUID, Object>> lastSaved = Collections.synchronizedMap(new HashMap<>());

    @SuppressWarnings("unchecked")
    public <T> T get(UUID identifier, String name) {
        T value = (T) data.getOrDefault(identifier, new HashMap<>()).get(name);
        if(value == null) {
            throw new IllegalStateException("Caught name not found: " + name);
        }
        return value;
    }

    public <T> T add(UUID identifier, String name, T value) {
        if(value == null) {
            return null;
        }
        if(!data.containsKey(identifier)) {
            data.put(identifier, Collections.synchronizedMap(new HashMap<>()));
        }
        data.get(identifier).put(name, value);
        lastSaved.put(name, new Pair<>(identifier, value));
        return value;
    }

    public <T> boolean contains(UUID identifier, String name) {
        return data.getOrDefault(identifier, new HashMap<>()).containsKey(name);
    }

    @SuppressWarnings("unchecked")
    public <T> T remove(UUID identifier, String name) {
        Map<String, Object> dataByType = data.getOrDefault(identifier, new HashMap<>());
        T value = (T) dataByType.get(name);
        dataByType.remove(name);
        return value;
    }

    @SuppressWarnings("unchecked")
    public <T> T getLastSaved(String name) {
        return (T) lastSaved.get(name).getRight();
    }
}
