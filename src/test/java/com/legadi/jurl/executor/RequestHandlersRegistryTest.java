package com.legadi.jurl.executor;

import static com.legadi.jurl.executor.RequestHandlersRegistry.findExecutorByRequestType;
import static com.legadi.jurl.executor.RequestHandlersRegistry.findProcessorByRequestType;
import static com.legadi.jurl.executor.RequestHandlersRegistry.registerHandler;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
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

        RequestExecutor<?, ?> executor = Assertions.assertDoesNotThrow(
            () -> findExecutorByRequestType("test"));
        ResponseProcessor<?, ?> processor = Assertions.assertDoesNotThrow(
            () -> findProcessorByRequestType("test"));

        Assertions.assertNotNull(executor);
        Assertions.assertNotNull(processor);
    }

    @Test
    public void notFound() {
        Assertions.assertThrows(CommandException.class,
            () -> findExecutorByRequestType("not-found"));
        Assertions.assertThrows(CommandException.class,
            () -> findProcessorByRequestType("not-found"));
    }

    public static class TestRequestExecutor implements RequestExecutor<TestRequest, TestResponse> {

        @Override
        public String type() {
            return "test";
        }

        @Override
        public TypeToken<TestRequest> requestType() {
            return new TypeToken<TestRequest>() {};
        }

        @Override
        public Optional<AssertionResult> acceptsConditions(Settings settings, TestRequest request) {
            return Optional.empty();
        };

        @Override
        public TestResponse executeRequest(Settings settings, String requestInputPath,
                TestRequest request) throws RequestException {
            return new TestResponse();
        }

        @Override
        public void mergeAPIDefinition(Settings settings, TestRequest api, TestRequest request) {
        }

        @Override
        public void mergeBodyFileWithBodyContent(Settings settings, String requestInputPath,
            TestRequest request) {
        }

        @Override
        public void overrideRequestWithFile(Settings settings, TestRequest request, String filename) {
        }

        @Override
        public Map<String, Object> getDetailsFromResponse(TestResponse response) {
            return new HashMap<>();
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
