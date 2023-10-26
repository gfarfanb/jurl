package com.legadi.jurl.embedded.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;

public class RequestCatcher {

    private final Map<UUID, Map<String, Object>> data = Collections.synchronizedMap(new HashMap<>());
    private final Map<String, List<Pair<UUID, Object>>> history = Collections.synchronizedMap(new HashMap<>());
    private final AtomicReference<UUID> lastCorrelationId = new AtomicReference<UUID>();

    @SuppressWarnings("unchecked")
    public <T> T get(UUID correlationId, String name) {
        T value = (T) data.getOrDefault(correlationId, new HashMap<>()).get(name);
        if(value == null) {
            throw new IllegalStateException("Caught name not found: " + name);
        }
        return value;
    }

    public <T> T add(UUID correlationId, String name, T value) {
        if(value == null) {
            return null;
        }
        if(!data.containsKey(correlationId)) {
            data.put(correlationId, Collections.synchronizedMap(new HashMap<>()));
        }
        data.get(correlationId).put(name, value);

        List<Pair<UUID, Object>> records = history.get(name);
        if(records == null) {
            records = new LinkedList<>();
        }
        records.add(new Pair<>(correlationId, value));
        history.put(name, records);
        lastCorrelationId.set(correlationId);

        return value;
    }

    public <T> boolean contains(UUID correlationId, String name) {
        return data.getOrDefault(correlationId, new HashMap<>()).containsKey(name);
    }

    @SuppressWarnings("unchecked")
    public <T> T remove(UUID correlationId, String name) {
        Map<String, Object> dataByType = data.getOrDefault(correlationId, new HashMap<>());
        T value = (T) dataByType.get(name);
        dataByType.remove(name);
        return value;
    }

    public UUID getLastCorrelationId() {
        return lastCorrelationId.get();
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
