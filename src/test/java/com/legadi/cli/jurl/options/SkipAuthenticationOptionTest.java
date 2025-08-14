package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_AUTHENTICATION;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.util.AuthenticationCleaner;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class SkipAuthenticationOptionTest extends OptionAbstractTest<SkipAuthenticationOption> {

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

        Settings authSettings = requestCatcher.getLast(authCorrelationId, SETTINGS);
        List<HTTPResponseEntry> authResponses = requestCatcher.getAll(authCorrelationId, RESPONSE);

        Assertions.assertEquals(Boolean.FALSE.toString(), authSettings.get(PROP_SKIP_AUTHENTICATION));
        Assertions.assertEquals(2, authResponses.size());
        Assertions.assertEquals(200, authResponses.get(0).getStatusCode());
        Assertions.assertEquals(201, authResponses.get(1).getStatusCode());

        AuthenticationCleaner.cleanup(requestCatcher, authCorrelationId);

        UUID skipCorrelationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-na",
                "-n", "create",
                "src/test/resources/skip-auth-request.spec.http"
            ));

        Settings skipSettings = requestCatcher.getLast(skipCorrelationId, SETTINGS);
        List<HTTPResponseEntry> skipResponses = requestCatcher.getAll(skipCorrelationId, RESPONSE);

        Assertions.assertEquals(Boolean.TRUE.toString(), skipSettings.get(PROP_SKIP_AUTHENTICATION));
        Assertions.assertEquals(1, skipResponses.size());
        Assertions.assertEquals(201, skipResponses.get(0).getStatusCode());

        AuthenticationCleaner.cleanup(requestCatcher, skipCorrelationId);
    }

    @Test
    public void skipAuthenticationByDefinedOpt() {
        UUID authCorrelationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-n", "skipped-auth-by-opt",
                "src/test/resources/skip-auth-request.spec.http"
            ));

        Settings authSettings = requestCatcher.getLast(authCorrelationId, SETTINGS);
        List<HTTPResponseEntry> authResponses = requestCatcher.getAll(authCorrelationId, RESPONSE);

        Assertions.assertEquals(Boolean.TRUE.toString(), authSettings.get(PROP_SKIP_AUTHENTICATION));
        Assertions.assertEquals(1, authResponses.size());
        Assertions.assertEquals(201, authResponses.get(0).getStatusCode());

        AuthenticationCleaner.cleanup(requestCatcher, authCorrelationId);
    }

    @Test
    public void skipAuthenticationBySetValueOpt() {
        UUID authCorrelationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-s", "skipAuthentication", "true",
                "-n", "create",
                "src/test/resources/skip-auth-request.spec.http"
            ));

        Settings authSettings = requestCatcher.getLast(authCorrelationId, SETTINGS);
        List<HTTPResponseEntry> authResponses = requestCatcher.getAll(authCorrelationId, RESPONSE);

        Assertions.assertEquals(Boolean.TRUE.toString(), authSettings.get(PROP_SKIP_AUTHENTICATION));
        Assertions.assertEquals(1, authResponses.size());
        Assertions.assertEquals(201, authResponses.get(0).getStatusCode());

        AuthenticationCleaner.cleanup(requestCatcher, authCorrelationId);
    }
}
