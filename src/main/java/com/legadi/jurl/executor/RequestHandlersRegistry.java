package com.legadi.jurl.executor;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;

public class RequestHandlersRegistry {

    private static final List<Pair<Supplier<RequestExecutor<?, ?>>, Supplier<ResponseProcessor<?, ?>>>> EXECUTORS = new LinkedList<>();

    static {
        registerHandler(HTTPRequestExecutor::new, HTTPResponseProcessor::new);
    }

    public static void registerHandler(String executorClass, String processorClass) {
        registerHandler(() -> instantiate(executorClass), () -> instantiate(processorClass));
    }

    public static void registerHandler(Supplier<RequestExecutor<?, ?>> requestExecutorSupplier,
            Supplier<ResponseProcessor<?, ?>> responseProcessorSupplier) {
        EXECUTORS.add(new Pair<>(requestExecutorSupplier, responseProcessorSupplier));
    }

    public static Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> findByRequest(
            RequestEntry<? extends MockEntry> request) {
        List<Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>>> executors = EXECUTORS
            .stream()
            .map(pair -> new Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>>(
                pair.getLeft().get(), pair.getRight().get()
            ))
            .filter(pair -> pair.getLeft().accepts(request))
            .collect(Collectors.toCollection(ArrayList::new));

        if(executors.isEmpty()) {
            throw new CommandException("Unable to obtain request executor for:" + request);
        }

        Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> lastExecutor = executors.get(executors.size() - 1);
        return lastExecutor;
    }
}
