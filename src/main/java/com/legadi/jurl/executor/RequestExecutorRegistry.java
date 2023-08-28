package com.legadi.jurl.executor;

import java.util.LinkedList;
import java.util.List;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.RequestEntry;

public class RequestExecutorRegistry {

    private static final List<RequestExecutor<?, ?>> EXECUTORS = new LinkedList<>();

    static {
        EXECUTORS.add(new HTTPRequestExecutor());
    }

    public static RequestExecutor<?, ?> getExecutor(RequestEntry request) {
        return EXECUTORS
            .stream()
            .filter(executor -> executor.accepts(request))
            .findFirst()
            .orElseThrow(() -> new CommandException("Unable to obtain executor"));
    }
}
