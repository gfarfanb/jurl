package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.embedded.util.AuthenticationCleaner;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class StartInStepOptionTest extends OptionAbstractTest<StartInStepOption> {

    public StartInStepOptionTest() {
        super("--start-in");
    }

    @Test
    public void startInStepIndexValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-si", "2",
                "-n", "basicWithQuery",
                "src/test/resources/flow.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, REQUEST);
        List<HTTPResponseEntry> responses = requestCatcher.getAll(correlationId, RESPONSE);

        Assertions.assertEquals(5, requests.size());
        Assertions.assertEquals(5, responses.size());

        Assertions.assertEquals("create/token-authorization", requests.get(0).getName());
        Assertions.assertEquals("create", requests.get(1).getName());
        Assertions.assertEquals("obtain", requests.get(2).getName());
        Assertions.assertEquals("update", requests.get(3).getName());
        Assertions.assertEquals("remove", requests.get(4).getName());

        AuthenticationCleaner.cleanup(requestCatcher, correlationId);
    }

    @Test
    public void startInStepWrongIndexValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-si", "1000",
                "-n", "basicWithQuery",
                "src/test/resources/flow.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, REQUEST);
        List<HTTPResponseEntry> responses = requestCatcher.getAll(correlationId, RESPONSE);

        Assertions.assertTrue(requests.isEmpty());
        Assertions.assertTrue(responses.isEmpty());
    }

    @Test
    public void startInStepNameValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-si", "create",
                "-n", "basicWithQuery",
                "src/test/resources/flow.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, REQUEST);
        List<HTTPResponseEntry> responses = requestCatcher.getAll(correlationId, RESPONSE);

        Assertions.assertEquals(5, requests.size());
        Assertions.assertEquals(5, responses.size());

        Assertions.assertEquals("create/token-authorization", requests.get(0).getName());
        Assertions.assertEquals("create", requests.get(1).getName());
        Assertions.assertEquals("obtain", requests.get(2).getName());
        Assertions.assertEquals("update", requests.get(3).getName());
        Assertions.assertEquals("remove", requests.get(4).getName());

        AuthenticationCleaner.cleanup(requestCatcher, correlationId);
    }

    @Test
    public void startInStepWrongNameValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-si", "not-found",
                "-n", "basicWithQuery",
                "src/test/resources/flow.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, REQUEST);
        List<HTTPResponseEntry> responses = requestCatcher.getAll(correlationId, RESPONSE);

        Assertions.assertTrue(requests.isEmpty());
        Assertions.assertTrue(responses.isEmpty());
    }
}
