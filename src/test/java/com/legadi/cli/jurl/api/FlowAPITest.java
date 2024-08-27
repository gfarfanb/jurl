package com.legadi.cli.jurl.api;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPITest;
import com.legadi.cli.jurl.exception.RecursiveCommandException;
import com.legadi.cli.jurl.model.AssertionResult;

public class FlowAPITest extends EmbeddedAPITest {

    @Test
    public void authorization() {
        Settings settings = new Settings();
        Map<String, String> properties = new HashMap<>();

        properties.put("auth.expiration.millis", "0");

        Settings.mergeProperties("default", properties);

        String previousToken = settings.getOrDefault("auth.access.token", null);
        String previousExpirationMillis = settings.get("auth.expiration.millis");

        UUID authCorrelationId = jurl("-n", "authorization", "src/test/resources/flow.spec.http");
        Settings authSettings = requestCatcher.getLast(authCorrelationId, "settings");

        String currentToken = Assertions.assertDoesNotThrow(
            () -> authSettings.get("auth.access.token"));
        String currentExpirationMillis = Assertions.assertDoesNotThrow(
            () -> authSettings.get("auth.expiration.millis"));

        Assertions.assertNotEquals(previousToken, currentToken);
        Assertions.assertNotEquals(previousExpirationMillis, currentExpirationMillis);

        jurl("-n", "authorization", "src/test/resources/flow.spec.http");

        Optional<AssertionResult> skipped = requestCatcher
            .<Optional<AssertionResult>>getLastSaved("conditions-result")
            .getRight();;

        Assertions.assertTrue(skipped.isPresent());
        Assertions.assertFalse(skipped.get().isPassed());
    }

    @Test
    public void authorizationAndCRUDFlow() {
        String basicWithAuthorizationTag = UUID.randomUUID().toString();
        UUID commonCorrelationId = UUID.randomUUID();

        jurl(commonCorrelationId, "-n", "basicWithAuthorization",
            "-s", "basic.with.authorization.tag", basicWithAuthorizationTag,
            "src/test/resources/flow.spec.http");

        List<Pair<UUID, Settings>> settingsRecords = requestCatcher
            .getLastSaved("settings", 5);

        for(Pair<UUID, Settings> settingsRecord : settingsRecords) {
            Assertions.assertEquals(commonCorrelationId, settingsRecord.getLeft());
            Assertions.assertEquals(basicWithAuthorizationTag, settingsRecord.getRight().get("flow.tag"));
        }

        List<Pair<UUID, Optional<AssertionResult>>> assertionRecords = requestCatcher
            .getLastSaved("assertions-result", 5);

        for(Pair<UUID, Optional<AssertionResult>> assertionRecord : assertionRecords) {
            Assertions.assertEquals(commonCorrelationId, assertionRecord.getLeft());
            Assertions.assertTrue(assertionRecord.getRight().isPresent());
            Assertions.assertEquals(1, assertionRecord.getRight().get().getAssertions());
            Assertions.assertEquals(0, assertionRecord.getRight().get().getFailures());
            Assertions.assertTrue(assertionRecord.getRight().get().isPassed());
        }
    }

    @Test
    public void invalidRecursiveFlow() {
        Assertions.assertThrows(RecursiveCommandException.class,
            () -> jurl("-n", "invalidRecursiveFlow", "src/test/resources/flow.spec.http"));
    }
}
