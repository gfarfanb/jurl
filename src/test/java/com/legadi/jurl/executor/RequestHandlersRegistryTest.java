package com.legadi.jurl.executor;

import static com.legadi.jurl.executor.RequestHandlersRegistry.findByRequest;
import static com.legadi.jurl.executor.RequestHandlersRegistry.registerHandler;

import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.MockEntry;
import com.legadi.jurl.model.RequestEntry;
import com.legadi.jurl.model.ResponseEntry;

public class RequestHandlersRegistryTest {

    @Test
    public void registerHandlerCustom() {
        registerHandler(TestRequestExecutor.class.getName(), TestResponseProcessor.class.getName());

        Pair<RequestExecutor<?, ?>, ResponseProcessor<?, ?>> handlers = Assertions.assertDoesNotThrow(
            () -> findByRequest(new TestRequest()));

        Assertions.assertNotNull(handlers);
        Assertions.assertNotNull(handlers.getLeft());
        Assertions.assertNotNull(handlers.getRight());
    }

    @Test
    public void notFound() {
        Assertions.assertThrows(CommandException.class,
            () -> findByRequest(new RequestEntry<MockEntry>()));
    }

    public static class TestRequestExecutor implements RequestExecutor<TestRequest, TestResponse> {

        @Override
        public boolean accepts(RequestEntry<? extends MockEntry> request) {
            return request instanceof TestRequest;
        }

        @Override
        public TypeToken<TestRequest> type() {
            return new TypeToken<TestRequest>() {};
        }

        @Override
        public TestResponse executeRequest(Settings settings, TestRequest request) throws RequestException {
            return new TestResponse();
        }

        @Override
        public void mergeAPIDefinition(Settings settings, TestRequest api, TestRequest request) {
        }

        @Override
        public void mergeBodyFileWithBodyContent(Settings settings, TestRequest request) {
        }

        @Override
        public void overrideRequestWithFile(Settings settings, TestRequest request, String filename) {
        }
    }

    public static class TestResponseProcessor implements ResponseProcessor<TestRequest, TestResponse> {

        @Override
        public Optional<AssertionResult> processResponse(Settings settings, TestRequest request, TestResponse response)
                throws RequestException {
            return Optional.empty();
        }
    }

    public static class TestRequest extends RequestEntry<MockEntry> {

    }

    public static class TestResponse extends ResponseEntry {

    }
}
