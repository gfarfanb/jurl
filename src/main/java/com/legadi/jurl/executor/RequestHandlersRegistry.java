package com.legadi.jurl.executor;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.executor.http.HTTPRequestModifier;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;

public class RequestHandlersRegistry {

    private static final Map<String, Supplier<RequestModifier<?, ?>>> MODIFIERS = new HashMap<>();
    private static final Map<String, Supplier<RequestExecutor<?, ?>>> EXECUTORS = new HashMap<>();
    private static final Map<String, Supplier<ResponseProcessor<?, ?>>> PROCESSORS = new HashMap<>();

    static {
        registerModifier(HTTPRequestModifier::new);
        registerExecutor(HTTPRequestExecutor::new);
        registerProcessor(HTTPResponseProcessor::new);
    }

    private RequestHandlersRegistry() {}

    public static void registerModifier(String modifierClass) {
        registerModifier(() -> instantiate(modifierClass));
    }

    public static void registerExecutor(String executorClass) {
        registerExecutor(() -> instantiate(executorClass));
    }

    public static void registerProcessor(String processorClass) {
        registerProcessor(() -> instantiate(processorClass));
    }

    public static void registerModifier(Supplier<RequestModifier<?, ?>> requestModifierSupplier) {
        RequestModifier<?, ?> modifier = requestModifierSupplier.get();
        MODIFIERS.put(modifier.type(), requestModifierSupplier);
    }

    public static void registerExecutor(Supplier<RequestExecutor<?, ?>> requestExecutorSupplier) {
        RequestExecutor<?, ?> executor = requestExecutorSupplier.get();
        EXECUTORS.put(executor.type(), requestExecutorSupplier);
    }

    public static void registerProcessor(Supplier<ResponseProcessor<?, ?>> responseProcessorSupplier) {
        ResponseProcessor<?, ?> processor = responseProcessorSupplier.get();
        PROCESSORS.put(processor.type(), responseProcessorSupplier);
    }

    public static RequestModifier<?, ?> findModifierByRequestType(String requestType) {
        Supplier<RequestModifier<?, ?>> modifier = MODIFIERS.get(requestType);

        if(modifier == null) {
            throw new CommandException("Unable to obtain request modifier for: " + requestType);
        }

        return modifier.get();
    }

    public static RequestExecutor<?, ?> findExecutorByRequestType(String requestType) {
        Supplier<RequestExecutor<?, ?>> executor = EXECUTORS.get(requestType);

        if(executor == null) {
            throw new CommandException("Unable to obtain request executor for: " + requestType);
        }

        return executor.get();
    }

    public static ResponseProcessor<?, ?> findProcessorByRequestType(
            String requestType) {
        Supplier<ResponseProcessor<?, ?>> executor = PROCESSORS.get(requestType);

        if(executor == null) {
            throw new CommandException("Unable to obtain response processor for:" + requestType);
        }

        return executor.get();
    }
}
