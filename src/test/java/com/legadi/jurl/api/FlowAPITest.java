package com.legadi.jurl.api;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;

public class FlowAPITest extends EmbeddedAPITest {

    @Test
    public void authorization() {
        Settings settings = new Settings();
        String previousToken = settings.getOrDefault("auth.access.token", null);
        String previousExpirationMillis = settings.getOrDefault("auth.expiration.millis", null);

        UUID authIdentifier = jurl("src/test/resources/authorization.spec.http");
        Settings authSettings = requestCatcher.get(authIdentifier, "settings");

        String currentToken = Assertions.assertDoesNotThrow(
            () -> authSettings.get("auth.access.token"));
        String currentExpirationMillis = Assertions.assertDoesNotThrow(
            () -> authSettings.get("auth.expiration.millis"));

        Assertions.assertNotEquals(previousToken, currentToken);
        Assertions.assertNotEquals(previousExpirationMillis, currentExpirationMillis);
    }
}
