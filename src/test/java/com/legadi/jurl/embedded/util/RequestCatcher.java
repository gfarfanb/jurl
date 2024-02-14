package com.legadi.jurl.embedded.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;

public class RequestCatcher {

    private final Map<UUID, Map<String, List<Object>>> data = Collections.synchronizedMap(new HashMap<>());
    private final Map<String, List<Pair<UUID, Object>>> history = Collections.synchronizedMap(new HashMap<>());

    public <T> T get(UUID correlationId, String name) {
        List<T> values = getAll(correlationId, name);
        if(values.isEmpty()) {
            throw new IllegalStateException("Caught name not found: " + name);
        }
        return values.get(0);
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> getAll(UUID correlationId, String name) {
        return (List<T>) data
            .getOrDefault(correlationId, new HashMap<>())
            .getOrDefault(name, new LinkedList<>());
    }

    public <T> T add(UUID correlationId, String name, T value) {
        if(value == null) {
            return null;
        }
        if(!data.containsKey(correlationId)) {
            data.put(correlationId, Collections.synchronizedMap(new HashMap<>()));
        }
        if(!data.get(correlationId).containsKey(name)) {
            data.get(correlationId).put(name, new LinkedList<>());
        }
        data.get(correlationId).get(name).add(value);

        List<Pair<UUID, Object>> records = history.get(name);
        if(records == null) {
            records = new LinkedList<>();
        }
        records.add(new Pair<>(correlationId, value));
        history.put(name, records);

        return value;
    }

    public boolean contains(UUID correlationId, String name) {
        return data.getOrDefault(correlationId, new HashMap<>()).containsKey(name);
    }

    @SuppressWarnings("unchecked")
    public <T> T remove(UUID correlationId, String name) {
        Map<String, List<Object>> dataByType = data.getOrDefault(correlationId, new HashMap<>());
        List<T> values = (List<T>) dataByType.getOrDefault(name, new LinkedList<>());
        dataByType.remove(name);
        return !values.isEmpty() ? values.get(0) : null;
    }

    @SuppressWarnings("unchecked")
    public <T> Pair<UUID, T> getLastSaved(String name) {
        List<Pair<UUID, Object>> records = getHistoryRecords(name);
        return (Pair<UUID, T>) records.get(records.size() - 1);
    }

    @SuppressWarnings("unchecked")
    public <T> List<Pair<UUID, T>> getLastSaved(String name, int elements) {
        List<Pair<UUID, Object>> records = getHistoryRecords(name);
        if(records.size() < elements) {
            throw new IllegalStateException("Not enough records for: " + name
                + " - expectedElements=" + elements + " currentSize=" + records.size());
        }
        return records.subList(records.size() - elements, records.size())
            .stream()
            .map(r -> (Pair<UUID, T>) r)
            .collect(Collectors.toList());
    }

    private List<Pair<UUID, Object>> getHistoryRecords(String name) {
        List<Pair<UUID, Object>> records = history.get(name);
        if(records == null) {
            throw new IllegalStateException("Records not found for: " + name);
        }
        return records;
    }
}
