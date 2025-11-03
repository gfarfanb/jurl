package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_EXECUTE_AUTHENTICATION;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.util.AuthenticationCleaner;

public class ExecuteAuthenticationOptionTest extends OptionAbstractTest<ExecuteAuthenticationOption> {

    public ExecuteAuthenticationOptionTest() {
        super("--exec-auth", true);
    }

    @Test
    public void executeAuthenticationValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-n", "create", "-ea",
                "src/test/resources/auth-request.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_EXECUTE_AUTHENTICATION));

        AuthenticationCleaner.cleanup(requestCatcher, correlationId);
    }
}
