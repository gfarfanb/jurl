package com.legadi.jurl.common;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.legadi.jurl.exception.RecursiveCommandException;

public class ExecutionTrace {

    private static final String TAB = "    ";

    private final List<String> trace = new LinkedList<>();
    private final Lock lock = new ReentrantLock();

    public ExecutionTrace nextIteration() {
        ExecutionTrace executionTrace = new ExecutionTrace();
        executionTrace.trace.addAll(trace);
        return executionTrace;
    }

    public void validateExecution(String requestInputPath, String inputName) {
        lock.lock();

        try {
            String command = requestInputPath + "/" + inputName;

            if(trace.contains(command)) {
                trace.add(command);
                throw new RecursiveCommandException("Request input was already processed: "
                    + requestInputPath + "/" + inputName + " - trace:\n" + printTrace());
            } else {
                trace.add(command);
            }
        } finally {
            lock.unlock();
        }
    }

    private String printTrace() {
        StringBuilder output = new StringBuilder();
        String tab = "";

        for(String command : trace) {
            output.append(tab).append("-> ").append(command).append("\n");
            tab += TAB;
        }

        return output.toString();
    }
}
