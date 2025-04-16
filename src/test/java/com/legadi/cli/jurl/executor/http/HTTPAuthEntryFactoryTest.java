package com.legadi.cli.jurl.executor.http;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPAuthEntryFactoryTest {

    @Test
    public void instanceTokenAuthValidation() {
        Settings settings = new Settings();
        HTTPAuthEntryFactory factory = new HTTPAuthEntryFactory(settings);

        HTTPTokenAuthEntry authEntry = Assertions.assertDoesNotThrow(
            () -> factory.instanceTokenAuth());

        Assertions.assertEquals("client_credentials", authEntry.getGrantType());
    }
}
