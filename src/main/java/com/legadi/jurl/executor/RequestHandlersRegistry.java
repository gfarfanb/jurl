package com.legadi.jurl.executor;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;

public class RequestHandlersRegistry {

    private static final List<Handler> EXECUTORS = new LinkedList<>();

    static {
        registerHandler(HTTPRequestExecutor::new, HTTPResponseProcessor::new);
    }

    public static void registerHandler(String executorClass, String processorClass) {
        Supplier<RequestExecutor<?, ?>> executorSupplier = () -> instantiate(executorClass);
        Supplier<ResponseProcessor<?, ?>> processorSupplier = () -> instantiate(processorClass);

        registerHandler(executorSupplier, processorSupplier);
    }

    public static void registerHandler(Supplier<RequestExecutor<?, ?>> requestExecutorSupplier,
            Supplier<ResponseProcessor<?, ?>> responseProcessorSupplier) {
        RequestExecutor<?, ?> requestExecutor = requestExecutorSupplier.get();
        Handler handler = new Handler();

        handler.setAccepts(requestExecutor::accepts);
        handler.setRequestExecutor(requestExecutorSupplier);
        handler.setResponseProcessor(responseProcessorSupplier);

        EXECUTORS.add(handler);
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    public static Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> findByRequest(
            RequestEntry<? extends MockEntry> request) {
        List<Handler> handlers = EXECUTORS
            .stream()
            .filter(handler -> handler.getAccepts().test(request))
            .collect(Collectors.toCollection(ArrayList::new));

        if(handlers.isEmpty()) {
            throw new CommandException("Unable to obtain request executor for:" + request);
        }

        Handler lastHandler = handlers.get(handlers.size() - 1);

        return new Pair(lastHandler.getRequestExecutor().get(), lastHandler.getResponseProcessor().get());
    }

    public static class Handler {

        private Predicate<RequestEntry<? extends MockEntry>> accepts;
        private Supplier<RequestExecutor<?, ?>> requestExecutor;
        private Supplier<ResponseProcessor<?, ?>> responseProcessor;

        public Predicate<RequestEntry<? extends MockEntry>> getAccepts() {
            return accepts;
        }

        public void setAccepts(Predicate<RequestEntry<? extends MockEntry>> accepts) {
            this.accepts = accepts;
        }

        public Supplier<RequestExecutor<?, ?>> getRequestExecutor() {
            return requestExecutor;
        }

        public void setRequestExecutor(Supplier<RequestExecutor<?, ?>> requestExecutor) {
            this.requestExecutor = requestExecutor;
        }

        public Supplier<ResponseProcessor<?, ?>> getResponseProcessor() {
            return responseProcessor;
        }

        public void setResponseProcessor(Supplier<ResponseProcessor<?, ?>> responseProcessor) {
            this.responseProcessor = responseProcessor;
        }
    }
}
