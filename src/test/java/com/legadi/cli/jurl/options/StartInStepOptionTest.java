package com.legadi.cli.jurl.options;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class StartInStepOptionTest extends OptionAbstractTest<StartInStepOption> {

    public StartInStepOptionTest() {
        super("--start-in");
    }

    @Test
    public void startInStepValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-si", "2",
                "-n", "basicWithAuthorization",
                "src/test/resources/flow.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");
        List<HTTPResponseEntry> responses = requestCatcher.getAll(correlationId, "response");

        Assertions.assertTrue(requests.stream().map(HTTPRequestEntry::getName)
            .noneMatch(n -> n.equalsIgnoreCase("authorization")));
        Assertions.assertEquals(4, requests.size());
        Assertions.assertEquals(4, responses.size());

        Assertions.assertEquals("create", requests.get(0).getName());
        Assertions.assertEquals("obtain", requests.get(1).getName());
        Assertions.assertEquals("update", requests.get(2).getName());
        Assertions.assertEquals("remove", requests.get(3).getName());
    }

    @Test
    public void wrongStartInStepDefinition() {
        Assertions.assertThrows(CommandException.class,
            () -> jurl(
                "-si", "two"
            ));
    }
}
