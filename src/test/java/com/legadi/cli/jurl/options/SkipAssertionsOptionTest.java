package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;
import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class SkipAssertionsOptionTest extends OptionAbstractTest<SkipAssertionsOption> {

    public SkipAssertionsOptionTest() {
        super("--not-assert", false);
    }

    @Test
    public void skipAssertionsValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-ns",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        Optional<AssertionResult> assertionResult = requestCatcher.getLast(correlationId, ASSERTIONS_RESULT);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_SKIP_ASSERTIONS));
        Assertions.assertFalse(assertionResult.isPresent());
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
