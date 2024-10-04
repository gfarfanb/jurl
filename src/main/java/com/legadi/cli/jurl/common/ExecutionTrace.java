package com.legadi.cli.jurl.common;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.legadi.cli.jurl.exception.RecursiveCommandException;

public class ExecutionTrace {

    private static final int TAB_FACTOR = 2;

    private final List<String> trace = new ArrayList<>();
    private final Lock lock = new ReentrantLock();
    private final Settings settings;

    public ExecutionTrace(Settings settings) {
        this.settings = settings;
    }

    public ExecutionTrace nextIteration() {
        ExecutionTrace executionTrace = new ExecutionTrace(settings);
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
            tab += String.format("%-" + (settings.getConsoleTabLength() + TAB_FACTOR) + "s", "");
        }

        return output.toString();
    }
}
