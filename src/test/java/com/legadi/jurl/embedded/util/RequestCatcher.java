package com.legadi.jurl.embedded.util;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;

public class RequestCatcher {

    private final Map<UUID, Map<String, List<Object>>> data = new HashMap<>();
    private final Map<String, List<Pair<UUID, Object>>> history = new HashMap<>();
    private final Lock lock = new ReentrantLock();

    public <T> T get(UUID correlationId, String name) {
        lock.lock();

        try {
            List<T> values = getAll(correlationId, name);
            if(values.isEmpty()) {
                throw new IllegalStateException("Caught name not found: " + name);
            }
            return values.get(0);
        } finally {
            lock.unlock();
        }
    }

    @SuppressWarnings("unchecked")
    public <T> List<T> getAll(UUID correlationId, String name) {
        lock.lock();

        try {
            return (List<T>) data
                .getOrDefault(correlationId, new HashMap<>())
                .getOrDefault(name, new LinkedList<>());
        } finally {
            lock.unlock();
        }
    }

    public <T> T add(UUID correlationId, String name, T value) {
        lock.lock();

        try {
            if(value == null) {
                return null;
            }
            if(!data.containsKey(correlationId)) {
                data.put(correlationId, new HashMap<>());
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
        } finally {
            lock.unlock();
        }
    }

    public boolean contains(UUID correlationId, String name) {
        lock.lock();

        try {
            return data.getOrDefault(correlationId, new HashMap<>()).containsKey(name);
        } finally {
            lock.unlock();
        }
    }

    @SuppressWarnings("unchecked")
    public <T> T remove(UUID correlationId, String name) {
        lock.lock();

        try {
            Map<String, List<Object>> dataByType = data.getOrDefault(correlationId, new HashMap<>());
            List<T> values = (List<T>) dataByType.getOrDefault(name, new LinkedList<>());
            dataByType.remove(name);
            return !values.isEmpty() ? values.get(0) : null;
        } finally {
            lock.unlock();
        }
    }

    @SuppressWarnings("unchecked")
    public <T> Pair<UUID, T> getLastSaved(String name) {
        lock.lock();

        try {
            List<Pair<UUID, Object>> records = getHistoryRecords(name);
            return (Pair<UUID, T>) records.get(records.size() - 1);
        } finally {
            lock.unlock();
        }
    }

    @SuppressWarnings("unchecked")
    public <T> List<Pair<UUID, T>> getLastSaved(String name, int elements) {
        lock.lock();

        try {
            List<Pair<UUID, Object>> records = getHistoryRecords(name);
            if(records.size() < elements) {
                throw new IllegalStateException("Not enough records for: " + name
                    + " - expectedElements=" + elements + " currentSize=" + records.size());
            }
            return records.subList(records.size() - elements, records.size())
                .stream()
                .map(r -> (Pair<UUID, T>) r)
                .collect(Collectors.toList());
        } finally {
            lock.unlock();
        }
    }

    private List<Pair<UUID, Object>> getHistoryRecords(String name) {
        List<Pair<UUID, Object>> records = history.get(name);
        if(records == null) {
            throw new IllegalStateException("Records not found for: " + name);
        }
        return records;
    }
}
