package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.register;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.common.annotations.Typed;
import com.legadi.cli.jurl.embedded.executor.HTTPRequestTestExecutor;
import com.legadi.cli.jurl.embedded.executor.HTTPResponseTestProcessor;
import com.legadi.cli.jurl.embedded.util.RequestCatcher;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.RequestExecutor;
import com.legadi.cli.jurl.executor.RequestModifier;
import com.legadi.cli.jurl.executor.ResponseProcessor;
import com.legadi.cli.jurl.executor.http.HTTPRequestModifier;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;
import com.legadi.cli.jurl.parser.HTTPRequestParser;
import com.legadi.cli.jurl.parser.RequestParser;

public class RequestTypeOptionTest extends OptionAbstractTest<RequestTypeOption> {

    public RequestTypeOptionTest() {
        super(RequestTypeOption.class, false);
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

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

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

    @Typed(type = "https")
    @Evaluable(values = { "https" }, op = Evaluable.Operation.EQUALS_IGNORE_CASE)
    public static class HTTPSParser extends HTTPRequestParser {

    }

    @Typed(type = "https")
    @Named(name = "https", allowOverride = true)
    public static class HTTPSModifier extends HTTPRequestModifier {

    }

    @Named(name = "https", allowOverride = true)
    public static class HTTPSExecutor extends HTTPRequestTestExecutor {

        public HTTPSExecutor(UUID identifier, RequestCatcher requestCatcher) {
            super(identifier, requestCatcher);
        }
    }

    @Named(name = "https", allowOverride = true)
    public static class HTTPSProcessor extends HTTPResponseTestProcessor {

        public HTTPSProcessor(UUID identifier, RequestCatcher requestCatcher) {
            super(identifier, requestCatcher);
        }
    }
}
