package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.CommonUtils.toGeneratedParam;
import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.JsonUtils.writeJsonFile;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPTokenHeaderAuthenticatorTest {

    @Test
    public void definitionValidation() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();

        Assertions.assertEquals(authenticator.type(), authenticator.type());
        Assertions.assertTrue(authenticator.requiresExecution());
        Assertions.assertNotNull(authenticator.getObjectFields());
        Assertions.assertFalse(authenticator.getObjectFields().isEmpty());
        Assertions.assertEquals("token", authenticator.getParserElement());
    }

    @Test
    public void instanceAuthEntryValidation() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        Settings settings = new Settings();

        HTTPTokenAuthEntry authEntry = Assertions.assertDoesNotThrow(
            () -> authenticator.instanceAuthEntry(settings));

        Assertions.assertEquals("token", authEntry.getParserElement());
        Assertions.assertEquals(settings.getAuthBearerGrantType(), authEntry.getGrantType());
        Assertions.assertNull(authEntry.getTokenUrl());
        Assertions.assertNull(authEntry.getClientId());
        Assertions.assertNull(authEntry.getClientSecret());
        Assertions.assertNull(authEntry.getScope());
    }

    @Test
    public void createAuthRequestEmpty() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        Settings settings = new Settings();

        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPRequestEntry request = new HTTPRequestEntry();

        Optional<HTTPRequestEntry> authRequest = Assertions.assertDoesNotThrow(
            () -> authenticator.createAuthRequest(settings, api, request, new HashMap<>()));

        Assertions.assertFalse(authRequest.isPresent());
    }

    @Test
    public void createAuthRequestWithAPI() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        Settings settings = new Settings();

        HTTPRequestEntry apiA = new HTTPRequestEntry();
        HTTPTokenAuthEntry apiAuthEntry = authenticator.instanceAuthEntry(settings);
        apiA.getAuthEntries().put(apiAuthEntry.getParserElement(), apiAuthEntry);

        HTTPRequestEntry requestA = new HTTPRequestEntry();

        Assertions.assertThrows(CommandException.class,
            () -> authenticator.createAuthRequest(settings, apiA, requestA, new HashMap<>()));

        HTTPRequestEntry apiB = new HTTPRequestEntry();
        apiAuthEntry = authenticator.instanceAuthEntry(settings);
        apiAuthEntry.setTokenUrl("http://localhost:5555/token");
        apiB.getAuthEntries().put(apiAuthEntry.getParserElement(), apiAuthEntry);

        HTTPRequestEntry requestB = new HTTPRequestEntry();

        Assertions.assertThrows(CommandException.class,
            () -> authenticator.createAuthRequest(settings, apiB, requestB, new HashMap<>()));

        HTTPRequestEntry apiC = new HTTPRequestEntry();
        apiAuthEntry = authenticator.instanceAuthEntry(settings);
        apiAuthEntry.setTokenUrl("http://localhost:5555/token");
        apiAuthEntry.setClientId("client-id");
        apiC.getAuthEntries().put(apiAuthEntry.getParserElement(), apiAuthEntry);

        HTTPRequestEntry requestC = new HTTPRequestEntry();

        Assertions.assertThrows(CommandException.class,
            () -> authenticator.createAuthRequest(settings, apiC, requestC, new HashMap<>()));

        HTTPRequestEntry apiD = new HTTPRequestEntry();
        apiAuthEntry = authenticator.instanceAuthEntry(settings);
        apiAuthEntry.setTokenUrl("http://localhost:5555/token");
        apiAuthEntry.setClientId("client-id");
        apiAuthEntry.setClientSecret("client-secret");
        apiD.getAuthEntries().put(apiAuthEntry.getParserElement(), apiAuthEntry);

        HTTPRequestEntry requestD = new HTTPRequestEntry();

        Assertions.assertThrows(CommandException.class,
            () -> authenticator.createAuthRequest(settings, apiD, requestD, new HashMap<>()));

        HTTPRequestEntry apiE = new HTTPRequestEntry();
        apiAuthEntry = authenticator.instanceAuthEntry(settings);
        apiAuthEntry.setTokenUrl("http://localhost:5555/token");
        apiAuthEntry.setClientId("client-id");
        apiAuthEntry.setClientSecret("client-secret");
        apiAuthEntry.setScope("scope");
        apiE.getAuthEntries().put(apiAuthEntry.getParserElement(), apiAuthEntry);

        HTTPRequestEntry requestE = new HTTPRequestEntry();

        Optional<HTTPRequestEntry> authRequest = Assertions.assertDoesNotThrow(
            () -> authenticator.createAuthRequest(settings, apiE, requestE, new HashMap<>()));

        Assertions.assertTrue(authRequest.isPresent());
    }

    @Test
    public void createAuthRequestMerge() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        Settings settings = new Settings();

        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPTokenAuthEntry apiAuthEntry = authenticator.instanceAuthEntry(settings);
        api.getAuthEntries().put(apiAuthEntry.getParserElement(), apiAuthEntry);

        HTTPRequestEntry request = new HTTPRequestEntry();
        HTTPTokenAuthEntry requestAuthEntry = authenticator.instanceAuthEntry(settings);
        requestAuthEntry.setTokenUrl("http://localhost:5555/token");
        requestAuthEntry.setClientId("client-id");
        requestAuthEntry.setClientSecret("client-secret");
        requestAuthEntry.setScope("scope");
        request.getAuthEntries().put(requestAuthEntry.getParserElement(), requestAuthEntry);

        Optional<HTTPRequestEntry> authRequest = Assertions.assertDoesNotThrow(
            () -> authenticator.createAuthRequest(settings, api, request, new HashMap<>()));

        Assertions.assertTrue(authRequest.isPresent());

        authRequest = Assertions.assertDoesNotThrow(
            () -> authenticator.createAuthRequest(settings, new HTTPRequestEntry(), request, new HashMap<>()));

        Assertions.assertTrue(authRequest.isPresent());
    }

    @Test
    public void cleanupAuthValidation() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        Settings settings = new Settings();

        Assertions.assertDoesNotThrow(() -> authenticator.cleanupAuth(settings,
            new HTTPRequestEntry(), new HTTPRequestEntry()));

        Map<String, String> properties = loadJsonProperties(settings.getOverrideFilePath());
        String clientId = UUID.randomUUID().toString();

        properties.put(toGeneratedParam(authenticator.type(), clientId, "expiration-millis"), "5000");
        properties.put(toGeneratedParam(authenticator.type(), clientId, "access-token"), UUID.randomUUID().toString());
        properties.put(toGeneratedParam(authenticator.type(), clientId, "expires-in." + settings.getAuthBearerExpiresInTimeUnit()), "5");
        properties.put(toGeneratedParam(authenticator.type(), clientId, "expiration-date"), settings.getTimestamp().toString());

        writeJsonFile(settings.getOverrideFilePath(), properties);

        HTTPTokenAuthEntry authEntry = authenticator.instanceAuthEntry(settings);
        authEntry.setClientId(clientId);

        HTTPRequestEntry request = new HTTPRequestEntry();
        request.getAuthEntries().put(authEntry.getParserElement(), authEntry);

        Assertions.assertDoesNotThrow(() -> authenticator.cleanupAuth(settings,
            new HTTPRequestEntry(), request));

        properties = loadJsonProperties(settings.getOverrideFilePath());

        Assertions.assertNull(properties.get(toGeneratedParam(authenticator.type(), clientId, "expiration-millis")));
        Assertions.assertNull(properties.get(toGeneratedParam(authenticator.type(), clientId, "access-token")));
        Assertions.assertNull(properties.get(toGeneratedParam(authenticator.type(), clientId, "expires-in." + settings.getAuthBearerExpiresInTimeUnit())));
        Assertions.assertNull(properties.get(toGeneratedParam(authenticator.type(), clientId, "expiration-date")));
    }

    @Test
    public void mergeAuthEntryValidation() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPRequestEntry request = new HTTPRequestEntry();

        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));

        api.getAuthEntries().put("token", new HTTPTokenAuthEntry());
        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));
        api.getAuthEntries().clear();

        request.getAuthEntries().put("token", new HTTPTokenAuthEntry());
        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));
        request.getAuthEntries().clear();

        api.getAuthEntries().put("token", new HTTPTokenAuthEntry());
        request.getAuthEntries().put("token", new HTTPTokenAuthEntry());

        HTTPTokenAuthEntry apiAuthEntry = (HTTPTokenAuthEntry) api.getAuthEntries().get("token");
        apiAuthEntry.setTokenUrl("http://localhost:5555/token");
        apiAuthEntry.setGrantType("client_credentials");
        apiAuthEntry.setClientId("client-id");
        apiAuthEntry.setClientSecret("client-secret");
        apiAuthEntry.setScope("scope");

        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));

        HTTPTokenAuthEntry requestAuthEntry = (HTTPTokenAuthEntry) api.getAuthEntries().get("token");

        Assertions.assertEquals("http://localhost:5555/token", apiAuthEntry.getTokenUrl());
        Assertions.assertEquals("client_credentials", apiAuthEntry.getGrantType());
        Assertions.assertEquals("client-id", apiAuthEntry.getClientId());
        Assertions.assertEquals("client-secret", apiAuthEntry.getClientSecret());
        Assertions.assertEquals("scope", apiAuthEntry.getScope());

        apiAuthEntry = (HTTPTokenAuthEntry) api.getAuthEntries().get("token");
        apiAuthEntry.setTokenUrl("http://localhost:5555/token/api");
        apiAuthEntry.setGrantType("api-client_credentials");
        apiAuthEntry.setClientId("api-client-id");
        apiAuthEntry.setClientSecret("api-client-secret");
        apiAuthEntry.setScope("api-scope");

        requestAuthEntry = (HTTPTokenAuthEntry) request.getAuthEntries().get("token");
        requestAuthEntry.setTokenUrl("http://localhost:5555/token/request");
        requestAuthEntry.setGrantType("request-client_credentials");
        requestAuthEntry.setClientId("request-client-id");
        requestAuthEntry.setClientSecret("request-client-secret");
        requestAuthEntry.setScope("request-scope");

        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));

        Assertions.assertEquals("http://localhost:5555/token/request", requestAuthEntry.getTokenUrl());
        Assertions.assertEquals("request-client_credentials", requestAuthEntry.getGrantType());
        Assertions.assertEquals("request-client-id", requestAuthEntry.getClientId());
        Assertions.assertEquals("request-client-secret", requestAuthEntry.getClientSecret());
        Assertions.assertEquals("request-scope", requestAuthEntry.getScope());
    }

    @Test
    public void getAuthHeadersValidation() {
        HTTPTokenHeaderAuthenticator authenticator = new HTTPTokenHeaderAuthenticator();
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        List<Pair<String, String>> headers = Assertions.assertDoesNotThrow(
            () -> authenticator.getAuthHeaders(settings, request));

        Assertions.assertTrue(headers.isEmpty());

        HTTPTokenAuthEntry authEntry = new HTTPTokenAuthEntry();
        authEntry.setClientId(UUID.randomUUID().toString());

        request.getAuthEntries().put(authEntry.getParserElement(), authEntry);

        headers = Assertions.assertDoesNotThrow(
            () -> authenticator.getAuthHeaders(settings, request));

        Assertions.assertTrue(headers.isEmpty());

        String tokenParam = toGeneratedParam(authenticator.type(), authEntry.getClientId(), "access-token");
        String token = UUID.randomUUID().toString();

        settings.putOverride(tokenParam, token);

        headers = Assertions.assertDoesNotThrow(
            () -> authenticator.getAuthHeaders(settings, request));

        Pair<String, String> authorization = headers.get(0);

        Assertions.assertEquals("Authorization", authorization.getLeft());
        Assertions.assertEquals("Bearer " + token, authorization.getRight());
    }
}
