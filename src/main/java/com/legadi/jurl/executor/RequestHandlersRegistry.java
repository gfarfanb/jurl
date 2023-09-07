package com.legadi.jurl.executor;

import java.util.LinkedList;
import java.util.List;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.RequestEntry;

public class RequestHandlersRegistry {

    private static final List<Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>>> EXECUTORS = new LinkedList<>();

    static {
        EXECUTORS.add(new Pair<>(new HTTPRequestExecutor(), new HTTPResponseProcessor()));
    }

    public static Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> findByRequest(RequestEntry request) {
        return EXECUTORS
            .stream()
            .filter(executor -> executor.getLeft().accepts(request))
            .findFirst()
            .orElseThrow(() -> new CommandException("Unable to obtain request executor"));
    }
}
