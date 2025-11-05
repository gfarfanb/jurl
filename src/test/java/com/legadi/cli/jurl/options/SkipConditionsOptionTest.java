package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;
import static com.legadi.cli.jurl.embedded.util.ObjectName.CONDITIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class SkipConditionsOptionTest extends OptionAbstractTest<SkipConditionsOption> {

    public SkipConditionsOptionTest() {
        super(SkipConditionsOption.class, false);
    }

    @Test
    public void skipConditionsValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-nc",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        Optional<AssertionResult> conditionResult = requestCatcher.getLast(correlationId, CONDITIONS_RESULT);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_SKIP_CONDITIONS));
        Assertions.assertFalse(conditionResult.isPresent());
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
