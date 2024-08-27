package com.legadi.cli.jurl.embedded;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;

import com.legadi.cli.jurl.common.ObjectsRegistry;
import com.legadi.cli.jurl.embedded.executor.HTTPRequestDummyExecutor;
import com.legadi.cli.jurl.embedded.executor.HTTPResponseDummyProcessor;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;
import com.legadi.cli.jurl.embedded.util.RequestCatcherManager;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.executor.ResponseProcessor;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public abstract class DummyAPITest {

    protected final UUID requestCatcherId = UUID.randomUUID();
    protected final RequestCatcher requestCatcher = RequestCatcherManager.getCatcher(requestCatcherId.toString());
    protected final UUID correlationId = UUID.randomUUID();

    @BeforeEach
    public void setup() {
        ObjectsRegistry.register(RequestExecutor.class, 
            HTTPRequestDummyExecutor.class, correlationId, requestCatcher);
        ObjectsRegistry.register(ResponseProcessor.class,
            HTTPResponseDummyProcessor.class, correlationId, requestCatcher);

        requestCatcher.add(correlationId, "conditions-result", Optional.empty());
        requestCatcher.add(correlationId, "response", new HTTPResponseEntry());
        requestCatcher.add(correlationId, "assertions-result", Optional.empty());
        requestCatcher.add(correlationId, "request-with-exception", "");
        requestCatcher.add(correlationId, "executor-executed", false);
        requestCatcher.add(correlationId, "processor-executed", false);
    }
}
