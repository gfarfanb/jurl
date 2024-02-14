package com.legadi.jurl.options;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class SetValueOptionTest extends OptionTest<SetValueOption> {

    public SetValueOptionTest() {
        super("--set");
    }

    @Test
    public void setValueValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-s", "number", "5",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.get(correlationId, "settings");
        HTTPResponseEntry response = requestCatcher.get(correlationId, "response");

        Assertions.assertTrue(settings.containsUserInput("number"));
        Assertions.assertEquals("5", settings.get("number"));
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
