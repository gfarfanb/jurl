package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class SkipConditionsOptionTest extends OptionTest<SkipConditionsOption> {

    public SkipConditionsOptionTest() {
        super("--no-conditions");
    }

    @Test
    public void skipConditionsValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-nc",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, "settings");
        Optional<AssertionResult> conditionResult = requestCatcher.getLast(correlationId, "conditions-result");
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, "response");

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_SKIP_CONDITIONS));
        Assertions.assertFalse(conditionResult.isPresent());
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
