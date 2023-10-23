package com.legadi.jurl.executor;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;

public class RequestHandlersRegistry {

    private static final Map<String, Pair<Supplier<RequestExecutor<?, ?>>, Supplier<ResponseProcessor<?, ?>>>> HANDLERS = new HashMap<>();

    static {
        registerHandler(HTTPRequestExecutor::new, HTTPResponseProcessor::new);
    }

    private RequestHandlersRegistry() {}

    public static void registerHandler(String executorClass, String processorClass) {
        registerHandler(() -> instantiate(executorClass), () -> instantiate(processorClass));
    }

    public static void registerHandler(Supplier<RequestExecutor<?, ?>> requestExecutorSupplier,
            Supplier<ResponseProcessor<?, ?>> responseProcessorSupplier) {
        RequestExecutor<?, ?> executor = requestExecutorSupplier.get();
        HANDLERS.put(executor.type(), new Pair<>(requestExecutorSupplier, responseProcessorSupplier));
    }

    public static RequestExecutor<?, ?> findExecutorByRequestType(String requestType) {
        Pair<Supplier<RequestExecutor<?, ?>>, Supplier<ResponseProcessor<?, ?>>> executor =
            HANDLERS.get(requestType);

        if(executor == null) {
            throw new CommandException("Unable to obtain request executor for: " + requestType);
        }

        return executor.getLeft().get();
    }

    public static ResponseProcessor<?, ?> findProcessorByRequestType(
            String requestType) {
        Pair<Supplier<RequestExecutor<?, ?>>, Supplier<ResponseProcessor<?, ?>>> executor =
            HANDLERS.get(requestType);

        if(executor == null) {
            throw new CommandException("Unable to obtain response processor for:" + requestType);
        }

        return executor.getRight().get();
    }
}
