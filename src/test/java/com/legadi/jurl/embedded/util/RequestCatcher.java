package com.legadi.jurl.embedded.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.google.gson.reflect.TypeToken;

public class RequestCatcher {

    private final Map<TypeToken<?>, Map<UUID, Object>> data = Collections.synchronizedMap(new HashMap<>());
    private final Map<TypeToken<?>, Object> lastSaved = Collections.synchronizedMap(new HashMap<>());

    @SuppressWarnings("unchecked")
    public <T> T get(TypeToken<T> dataType, UUID identifier) {
        return (T) data.getOrDefault(dataType, new HashMap<>()).get(identifier);
    }

    public <T> T add(TypeToken<T> dataType, UUID identifier, T value) {
        if(!data.containsKey(dataType)) {
            data.put(dataType, Collections.synchronizedMap(new HashMap<>()));
        }
        data.get(dataType).put(identifier, value);
        lastSaved.put(dataType, value);
        return value;
    }

    public <T> boolean contains(TypeToken<T> dataType, UUID identifier) {
        return data.getOrDefault(dataType, new HashMap<>()).containsKey(identifier);
    }

    @SuppressWarnings("unchecked")
    public <T> T remove(TypeToken<T> dataType, UUID identifier) {
        Map<UUID, Object> dataByType = data.getOrDefault(dataType, new HashMap<>());
        T value = (T) dataByType.get(identifier);
        dataByType.remove(identifier);
        return value;
    }

    @SuppressWarnings("unchecked")
    public <T> T getLastSaved(TypeToken<T> dataType) {
        return (T) lastSaved.get(dataType);
    }
}
