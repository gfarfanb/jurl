package com.legadi.jurl.options;

import static com.legadi.jurl.common.ObjectsRegistry.register;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.executor.HTTPRequestTestExecutor;
import com.legadi.jurl.embedded.executor.HTTPResponseTestProcessor;
import com.legadi.jurl.embedded.util.RequestCatcher;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.executor.RequestExecutor;
import com.legadi.jurl.executor.RequestModifier;
import com.legadi.jurl.executor.ResponseProcessor;
import com.legadi.jurl.executor.http.HTTPRequestModifier;
import com.legadi.jurl.model.http.HTTPResponseEntry;
import com.legadi.jurl.parser.HTTPRequestParser;
import com.legadi.jurl.parser.RequestParser;

public class RequestTypeOptionTest extends OptionTest<RequestTypeOption> {

    public RequestTypeOptionTest() {
        super("--request-type");
    }

    @Test
    public void setRequestType() {
        UUID correlationId = UUID.randomUUID();

        register(RequestParser.class, HTTPSParser.class);
        register(RequestModifier.class, HTTPSModifier.class);
        register(RequestExecutor.class, HTTPSExecutor.class, correlationId, requestCatcher);
        register(ResponseProcessor.class, HTTPSProcessor.class, correlationId, requestCatcher);

        Assertions.assertDoesNotThrow(
            () -> jurl(
                "-rt", "https",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.get(correlationId, "settings");
        HTTPResponseEntry response = requestCatcher.get(correlationId, "response");

        Assertions.assertEquals("https", settings.getRequestType());
        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void unsupportedRequestType() {
        Assertions.assertThrows(CommandException.class,
            () -> jurl(
                "-rt", "unsupported",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));
    }

    public static class HTTPSParser extends HTTPRequestParser {

        @Override
        public String type() {
            return "https";
        }
    }

    public static class HTTPSModifier extends HTTPRequestModifier {

        @Override
        public String name() {
            return "https";
        }
    }

    public static class HTTPSExecutor extends HTTPRequestTestExecutor {

        public HTTPSExecutor(UUID identifier, RequestCatcher requestCatcher) {
            super(identifier, requestCatcher);
        }

        @Override
        public String name() {
            return "https";
        }
    }

    public static class HTTPSProcessor extends HTTPResponseTestProcessor {

        public HTTPSProcessor(UUID identifier, RequestCatcher requestCatcher) {
            super(identifier, requestCatcher);
        }

        @Override
        public String name() {
            return "https";
        }
    }
}
