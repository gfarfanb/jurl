package com.legadi.jurl.api;

import java.time.format.DateTimeFormatter;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.EmbeddedAPITest;

public class FlowAPITest extends EmbeddedAPITest {

    @Test
    public void authorization() {
        Settings settings = new Settings();
        String previousToken = settings.getOrDefault("auth.token", null);
        String previousExpirationDate = settings.getOrDefault("auth.expiration.date", null);

        UUID authIdentifier = jurl("src/test/resources/authorization.spec.http");
        Settings authSettings = requestCatcher.get(authIdentifier, "settings");

        String currentToken = Assertions.assertDoesNotThrow(
            () -> authSettings.get("auth.token"));
        String currentExpirationDate = Assertions.assertDoesNotThrow(
            () -> authSettings.get("auth.expiration.date"));

        Assertions.assertNotEquals(previousToken, currentToken);
        Assertions.assertNotEquals(previousExpirationDate, currentExpirationDate);
        Assertions.assertDoesNotThrow(() -> DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse(currentExpirationDate));
    }
}
