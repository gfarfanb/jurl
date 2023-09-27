package com.legadi.jurl.embedded.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Pair;

public class RequestCatcher {

    private final Map<TypeToken<?>, Map<UUID, Object>> data = Collections.synchronizedMap(new HashMap<>());
    private final Map<TypeToken<?>, Pair<UUID, Object>> lastSaved = Collections.synchronizedMap(new HashMap<>());

    @SuppressWarnings("unchecked")
    public <T> T get(TypeToken<T> dataType, UUID identifier) {
        return (T) data.getOrDefault(dataType, new HashMap<>()).get(identifier);
    }

    public <T> T add(TypeToken<T> dataType, UUID identifier, T value) {
        if(value == null) {
            return null;
        }
        if(!data.containsKey(dataType)) {
            data.put(dataType, Collections.synchronizedMap(new HashMap<>()));
        }
        data.get(dataType).put(identifier, value);
        lastSaved.put(dataType, new Pair<>(identifier, value));
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
    public <T> Pair<UUID, T> getLastSaved(TypeToken<T> dataType) {
        return (Pair<UUID, T>) lastSaved.get(dataType);
    }
}
