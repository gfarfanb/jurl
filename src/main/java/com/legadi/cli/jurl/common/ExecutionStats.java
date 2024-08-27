package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.model.ExecutionStatus.PARTIALLY;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import com.legadi.cli.jurl.model.ExecutionStatus;

public class ExecutionStats {

    private final Map<ExecutionStatus, Integer> stats = new HashMap<>();
    private final Lock lock = new ReentrantLock();
    private final int executions;

    public ExecutionStats(int executions) {
        this.executions = executions;
    }

    public int getExecutions() {
        return executions;
    }

    public void count(ExecutionStatus status) {
        lock.lock();

        try {
            int count = stats.getOrDefault(status, 0) + 1;
            stats.put(status, count);
        } finally {
            lock.unlock();
        }
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
            .map(e -> e.getKey() + "=" + e.getValue())
            .collect(Collectors.joining(" "));
        return "[" + summary + "]";
    }
}
