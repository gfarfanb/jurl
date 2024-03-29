package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;

import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.AssertionResult;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class SkipAssertionsOptionTest extends OptionTest<SkipAssertionsOption> {

    public SkipAssertionsOptionTest() {
        super("--not-assert");
    }

    @Test
    public void skipAssertionsValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-ns",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, "settings");
        Optional<AssertionResult> assertionResult = requestCatcher.getLast(correlationId, "assertions-result");
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, "response");

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_SKIP_ASSERTIONS));
        Assertions.assertFalse(assertionResult.isPresent());
        Assertions.assertEquals(201, response.getStatusCode());
    }
}
