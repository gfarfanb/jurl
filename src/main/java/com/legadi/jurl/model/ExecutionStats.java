package com.legadi.jurl.model;

import static com.legadi.jurl.model.ExecutionStatus.PARTIALLY;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class ExecutionStats {

    private final Map<ExecutionStatus, Integer> stats = new HashMap<>();
    private final int executions;

    public ExecutionStats(int executions) {
        this.executions = executions;
    }

    public int getExecutions() {
        return executions;
    }

    public synchronized void count(ExecutionStatus status) {
        int count = stats.getOrDefault(status, 0) + 1;
        stats.put(status, count);
    }

    public int getCount(ExecutionStatus status) {
        return stats.getOrDefault(status, 0);
    }

    public ExecutionStatus computeStatus() {
        for(ExecutionStatus status : ExecutionStatus.values()) {
            int count = getCount(status);
            if(count == executions) {
                return status;
            }
        }
        return PARTIALLY;
    }

    @Override
    public String toString() {
        String summary = stats.entrySet()
            .stream()
            .filter(e -> e.getValue().compareTo(0) > 0)
            .map(e -> e.getKey() + "=" + e.getValue())
            .collect(Collectors.joining(" "));
        return "[" + summary + "]";
    }
}
