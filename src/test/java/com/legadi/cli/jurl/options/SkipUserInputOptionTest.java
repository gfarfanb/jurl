package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_USER_INPUT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.BODY;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class SkipUserInputOptionTest extends OptionAbstractTest<SkipUserInputOption> {

    public SkipUserInputOptionTest() {
        super("--no-input");
    }

    @Test
    public void skipUserInputOptionValidation() {
        UUID inputsCorrelationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-ni",
                "-n", "inputs",
                "src/test/resources/skip-inputs.spec.http"
            ));

        Settings inputsSettings = requestCatcher.getLast(inputsCorrelationId, SETTINGS);
        HTTPResponseEntry inputsResponse = requestCatcher.getLast(inputsCorrelationId, RESPONSE);

        UUID id = UUID.fromString(inputsResponse.getResponseHeaders().get("Resource-ID"));
        Map<String, String> inputsBody = requestCatcher.getLast(id, BODY);

        Assertions.assertEquals(Boolean.TRUE.toString(), inputsSettings.get(PROP_SKIP_USER_INPUT));
        Assertions.assertTrue(inputsBody.containsKey("userInput"));
        Assertions.assertTrue(inputsBody.get("userInput").isEmpty());
        Assertions.assertTrue(inputsBody.containsKey("password"));
        Assertions.assertTrue(inputsBody.get("password").isEmpty());
    }
}
