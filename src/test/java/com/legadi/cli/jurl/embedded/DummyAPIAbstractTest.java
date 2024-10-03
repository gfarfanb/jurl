package com.legadi.cli.jurl.embedded;

import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.CONDITIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.EXECUTOR_EXECUTED;
import static com.legadi.cli.jurl.embedded.util.ObjectName.PROCESSOR_EXECUTED;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST_WITH_EXCEPTION;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;

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

public abstract class DummyAPIAbstractTest {

    protected final UUID requestCatcherId = UUID.randomUUID();
    protected final RequestCatcher requestCatcher = RequestCatcherManager.getCatcher(requestCatcherId.toString());
    protected final UUID correlationId = UUID.randomUUID();

    @BeforeEach
    public void setup() {
        ObjectsRegistry.register(RequestExecutor.class, 
            HTTPRequestDummyExecutor.class, correlationId, requestCatcher);
        ObjectsRegistry.register(ResponseProcessor.class,
            HTTPResponseDummyProcessor.class, correlationId, requestCatcher);

        requestCatcher.add(correlationId, CONDITIONS_RESULT, Optional.empty());
        requestCatcher.add(correlationId, RESPONSE, new HTTPResponseEntry());
        requestCatcher.add(correlationId, ASSERTIONS_RESULT, Optional.empty());
        requestCatcher.add(correlationId, REQUEST_WITH_EXCEPTION, "");
        requestCatcher.add(correlationId, EXECUTOR_EXECUTED, false);
        requestCatcher.add(correlationId, PROCESSOR_EXECUTED, false);
    }
}
