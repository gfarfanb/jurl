package com.legadi.cli.jurl.executor.http;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractTypedType;

import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.auth.HTTPBasicAuthEntry;

public class HTTPBasicHeaderAuthenticatorTest {

    @Test
    public void definitionValidation() {
        HTTPBasicHeaderAuthenticator authenticator = new HTTPBasicHeaderAuthenticator();

        Assertions.assertEquals("http", extractTypedType(authenticator));
        Assertions.assertFalse(authenticator.requiresExecution());
        Assertions.assertNotNull(authenticator.getObjectFields());
        Assertions.assertFalse(authenticator.getObjectFields().isEmpty());
        Assertions.assertEquals("basic", authenticator.getParserElement());
    }

    @Test
    public void instanceAuthEntryValidation() {
        HTTPBasicHeaderAuthenticator authenticator = new HTTPBasicHeaderAuthenticator();
        Settings settings = new Settings();

        HTTPBasicAuthEntry authEntry = Assertions.assertDoesNotThrow(
            () -> authenticator.instanceAuthEntry(settings));

        Assertions.assertEquals("basic", authEntry.getParserElement());
        Assertions.assertNull(authEntry.getUsername());
        Assertions.assertNull(authEntry.getPassword());
    }

    @Test
    public void createAuthRequestValidation() {
        HTTPBasicHeaderAuthenticator authenticator = new HTTPBasicHeaderAuthenticator();
        Settings settings = new Settings();

        Optional<HTTPRequestEntry> authRequest = Assertions.assertDoesNotThrow(
            () -> authenticator.createAuthRequest(settings, new HTTPRequestEntry(),
                new HTTPRequestEntry(), new HashMap<>()));

        Assertions.assertFalse(authRequest.isPresent());
    }

    @Test
    public void cleanupAuthValidation() {
        HTTPBasicHeaderAuthenticator authenticator = new HTTPBasicHeaderAuthenticator();
        Settings settings = new Settings();

        Assertions.assertDoesNotThrow(
            () -> authenticator.cleanupAuth(settings, new HTTPRequestEntry(),
                new HTTPRequestEntry()));
    }

    @Test
    public void mergeAuthEntryValidation() {
        HTTPBasicHeaderAuthenticator authenticator = new HTTPBasicHeaderAuthenticator();
        HTTPRequestEntry api = new HTTPRequestEntry();
        HTTPRequestEntry request = new HTTPRequestEntry();

        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));

        api.getAuthEntries().put("basic", new HTTPBasicAuthEntry());
        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));
        api.getAuthEntries().clear();

        request.getAuthEntries().put("basic", new HTTPBasicAuthEntry());
        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));
        request.getAuthEntries().clear();

        api.getAuthEntries().put("basic", new HTTPBasicAuthEntry());
        request.getAuthEntries().put("basic", new HTTPBasicAuthEntry());

        HTTPBasicAuthEntry apiAuthEntry = (HTTPBasicAuthEntry) api.getAuthEntries().get("basic");
        apiAuthEntry.setUsername("username");
        apiAuthEntry.setPassword("p4s5w0rd");

        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));

        HTTPBasicAuthEntry requestAuthEntry = (HTTPBasicAuthEntry) api.getAuthEntries().get("basic");

        Assertions.assertEquals("username", requestAuthEntry.getUsername());
        Assertions.assertEquals("p4s5w0rd", requestAuthEntry.getPassword());

        apiAuthEntry = (HTTPBasicAuthEntry) api.getAuthEntries().get("basic");
        apiAuthEntry.setUsername("api-username");
        apiAuthEntry.setPassword("api-p4s5w0rd");

        requestAuthEntry = (HTTPBasicAuthEntry) request.getAuthEntries().get("basic");
        requestAuthEntry.setUsername("request-username");
        requestAuthEntry.setPassword("request-p4s5w0rd");

        Assertions.assertDoesNotThrow(
            () -> authenticator.mergeAuthEntry(api, request));

        Assertions.assertEquals("request-username", requestAuthEntry.getUsername());
        Assertions.assertEquals("request-p4s5w0rd", requestAuthEntry.getPassword());
    }

    @Test
    public void getAuthHeadersValidation() {
        HTTPBasicHeaderAuthenticator authenticator = new HTTPBasicHeaderAuthenticator();
        HTTPRequestEntry request = new HTTPRequestEntry();
        Settings settings = new Settings();

        List<Pair<String, String>> headers = Assertions.assertDoesNotThrow(
            () -> authenticator.getAuthHeaders(settings, request));

        Assertions.assertTrue(headers.isEmpty());

        HTTPBasicAuthEntry authEntry = new HTTPBasicAuthEntry();
        authEntry.setUsername("username");
        authEntry.setPassword("p4s5w0rd");

        request.getAuthEntries().put(authEntry.getParserElement(), authEntry);

        headers = Assertions.assertDoesNotThrow(
            () -> authenticator.getAuthHeaders(settings, request));

        Pair<String, String> authorization = headers.get(0);

        String basicDecoded = authEntry.getUsername() + ":" + authEntry.getPassword();
        String basicValue = Base64.getEncoder().encodeToString(basicDecoded.getBytes());

        Assertions.assertEquals("Authorization", authorization.getLeft());
        Assertions.assertEquals("Basic " + basicValue, authorization.getRight());
    }
}
