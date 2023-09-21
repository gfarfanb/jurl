package com.legadi.jurl.executor;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.http.HTTPRequestExecutor;
import com.legadi.jurl.executor.http.HTTPResponseProcessor;
import com.legadi.jurl.model.RequestEntry;

public class RequestHandlersRegistry {

    private static final List<Handler> EXECUTORS = new LinkedList<>();

    static {
        EXECUTORS.add(createHandler(HTTPRequestExecutor::new, HTTPResponseProcessor::new));
    }

    private static Handler createHandler(Supplier<RequestExecutor<?, ?>> requestExecutor,
            Supplier<ResponseProcessor<?, ?>> responseProcessor) {
        RequestExecutor<?, ?> executor = requestExecutor.get();
        Handler handler = new Handler();
        handler.setAccepts(request -> executor.accepts(request));
        handler.setRequestExecutor(requestExecutor);
        handler.setResponseProcessor(responseProcessor);
        return handler;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    public static Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> findByRequest(RequestEntry request) {
        return EXECUTORS
            .stream()
            .filter(handler -> handler.getAccepts().test(request))
            .findFirst()
            .map(handler -> new Pair(handler.getRequestExecutor().get(), handler.getResponseProcessor().get()))
            .orElseThrow(() -> new CommandException("Unable to obtain request executor"));
    }

    public static class Handler {

        private Predicate<RequestEntry> accepts;
        private Supplier<RequestExecutor<?, ?>> requestExecutor;
        private Supplier<ResponseProcessor<?, ?>> responseProcessor;

        public Predicate<RequestEntry> getAccepts() {
            return accepts;
        }

        public void setAccepts(Predicate<RequestEntry> accepts) {
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
