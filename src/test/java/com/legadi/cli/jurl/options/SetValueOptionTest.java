package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class SetValueOptionTest extends OptionAbstractTest<SetValueOption> {

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

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertTrue(settings.containsUserInput("number"));
        Assertions.assertEquals("5", settings.get("number"));
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
