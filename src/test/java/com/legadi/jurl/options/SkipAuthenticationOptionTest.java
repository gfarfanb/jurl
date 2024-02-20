package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_SKIP_AUTHENTICATION;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class SkipAuthenticationOptionTest extends OptionTest<SkipAuthenticationOption> {

    public SkipAuthenticationOptionTest() {
        super("--not-auth");
    }

    @Test
    public void skipAuthenticationValidation() {
        UUID authCorrelationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-n", "create",
                "src/test/resources/skip-auth-request.spec.http"
            ));

        Settings authSettings = requestCatcher.get(authCorrelationId, "settings");
        List<HTTPResponseEntry> authResponses = requestCatcher.getAll(authCorrelationId, "response");

        Assertions.assertEquals(Boolean.FALSE.toString(), authSettings.get(PROP_SKIP_AUTHENTICATION));
        Assertions.assertEquals(2, authResponses.size());
        Assertions.assertEquals(200, authResponses.get(0).getStatusCode());
        Assertions.assertEquals(201, authResponses.get(1).getStatusCode());

        UUID skipCorrelationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-na",
                "-n", "create",
                "src/test/resources/skip-auth-request.spec.http"
            ));

        Settings skipSettings = requestCatcher.get(skipCorrelationId, "settings");
        List<HTTPResponseEntry> skipResponses = requestCatcher.getAll(skipCorrelationId, "response");

        Assertions.assertEquals(Boolean.TRUE.toString(), skipSettings.get(PROP_SKIP_AUTHENTICATION));
        Assertions.assertEquals(1, skipResponses.size());
        Assertions.assertEquals(201, skipResponses.get(0).getStatusCode());
    }
}
