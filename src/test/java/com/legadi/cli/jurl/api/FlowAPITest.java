package com.legadi.cli.jurl.api;

import static com.legadi.cli.jurl.common.CommonUtils.toGeneratedParam;
import static com.legadi.cli.jurl.embedded.util.ObjectName.ASSERTIONS_RESULT;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPIAbstractTest;
import com.legadi.cli.jurl.embedded.util.AuthenticationCleaner;
import com.legadi.cli.jurl.exception.RecursiveCommandException;
import com.legadi.cli.jurl.model.AssertionResult;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class FlowAPITest extends EmbeddedAPIAbstractTest {

    @Test
    public void authorization() {
        UUID authCorrelationId = jurl("-n", "basicWithAuthorization", "src/test/resources/flow.spec.http");
        Settings authSettings = requestCatcher.getLast(authCorrelationId, SETTINGS);
        List<HTTPRequestEntry> requests = requestCatcher.getAll(authCorrelationId, REQUEST);
        HTTPTokenAuthEntry authEntry = requests
            .stream()
            .filter(r -> !r.getName().contains("authorization"))
            .map(HTTPRequestEntry::getAuthEntries)
            .flatMap(List::stream)
            .filter(auth -> HTTPTokenAuthEntry.class.isAssignableFrom(auth.getClass()))
            .map(auth -> (HTTPTokenAuthEntry) auth)
            .findFirst()
            .get();

        String tokenParam = toGeneratedParam(authSettings.getRequestType(),
            authEntry.getClientId(), "access-token");
        String expirationMillisParam = toGeneratedParam(authSettings.getRequestType(),
            authEntry.getClientId(), "expiration-millis");
        String expirationDateParam = toGeneratedParam(authSettings.getRequestType(),
            authEntry.getClientId(), "expiration-date");
        String expirationTimeUnitParam = toGeneratedParam(authSettings.getRequestType(),
            authEntry.getClientId(), "expires-in.SECONDS");

        Assertions.assertDoesNotThrow(
            () -> authSettings.get(tokenParam));
        Assertions.assertDoesNotThrow(
            () -> authSettings.get(expirationMillisParam));
        Assertions.assertDoesNotThrow(
            () -> authSettings.get(expirationDateParam));
        Assertions.assertDoesNotThrow(
            () -> authSettings.get(expirationTimeUnitParam));

        AuthenticationCleaner.cleanup(requestCatcher, authCorrelationId);
    }

    @Test
    public void authorizationAndCRUDFlow() {
        String basicWithAuthorizationTag = UUID.randomUUID().toString();
        UUID commonCorrelationId = UUID.randomUUID();

        jurl(commonCorrelationId, "-n", "basicWithAuthorization",
            "-s", "basic.with.authorization.tag", basicWithAuthorizationTag,
            "src/test/resources/flow.spec.http");

        List<Pair<UUID, Settings>> settingsRecords = requestCatcher
            .getLastSaved(SETTINGS, 5);

        for(Pair<UUID, Settings> settingsRecord : settingsRecords) {
            Assertions.assertEquals(commonCorrelationId, settingsRecord.getLeft());
            Assertions.assertEquals(basicWithAuthorizationTag, settingsRecord.getRight().get("flow.tag"));
        }

        List<Pair<UUID, Optional<AssertionResult>>> assertionRecords = requestCatcher
            .getLastSaved(ASSERTIONS_RESULT, 5);

        for(Pair<UUID, Optional<AssertionResult>> assertionRecord : assertionRecords) {
            Assertions.assertEquals(commonCorrelationId, assertionRecord.getLeft());
            Assertions.assertTrue(assertionRecord.getRight().isPresent());
            Assertions.assertEquals(1, assertionRecord.getRight().get().getAssertions());
            Assertions.assertEquals(0, assertionRecord.getRight().get().getFailures());
            Assertions.assertTrue(assertionRecord.getRight().get().isPassed());
        }

        AuthenticationCleaner.cleanup(requestCatcher, commonCorrelationId);
    }

    @Test
    public void invalidRecursiveFlow() {
        Assertions.assertThrows(RecursiveCommandException.class,
            () -> jurl("-n", "invalidRecursiveFlow", "src/test/resources/flow.spec.http"));
    }
}
