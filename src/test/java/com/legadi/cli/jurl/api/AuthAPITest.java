package com.legadi.cli.jurl.api;

import static com.legadi.cli.jurl.embedded.util.ObjectName.BODY;
import static com.legadi.cli.jurl.embedded.util.ObjectName.REQUEST;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.embedded.EmbeddedAPIAbstractTest;
import com.legadi.cli.jurl.embedded.util.AuthenticationCleaner;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;

public class AuthAPITest extends EmbeddedAPIAbstractTest {

    @AfterEach
    public void cleanup() {
        AuthenticationCleaner.cleanup();
    }

    @Test
    public void requestWithOneAuthTimes() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-t", "2",
                "-n", "create",
                "src/test/resources/auth-request.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, REQUEST);

        Assertions.assertEquals(3, requests.size());

        String authBody = (String) requestCatcher.getFirstSaved(BODY).getRight();
        HTTPRequestEntry authRequest = requests.get(0);

        Assertions.assertEquals("create/token-authorization", authRequest.getName());

        Assertions.assertTrue(authBody.contains("grant_type="));
        Assertions.assertTrue(authBody.contains("client_id="));
        Assertions.assertTrue(authBody.contains("client_secret="));
        Assertions.assertTrue(authBody.contains("scope="));
        Assertions.assertTrue(authBody.contains("child_key="));
        Assertions.assertTrue(authBody.contains("child_secret="));

        List<HTTPRequestEntry> createRequests = requests.subList(1, requests.size());

        Assertions.assertEquals(2, createRequests.size());

        for(HTTPRequestEntry request : createRequests) {
            Assertions.assertEquals("create", request.getName());
        }
    }

    @Test
    public void requestWithAuthBeforeRequestTimes() {
        Map<String, String> overrides = new HashMap<>();
        overrides.put("skipAuthentication", "true");

        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                overrides,
                "-t", "2",
                "-n", "create-auth",
                "src/test/resources/auth-request.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, REQUEST);

        Assertions.assertEquals(3, requests.size());

        String authBody = (String) requestCatcher.getFirstSaved(BODY).getRight();
        HTTPRequestEntry authRequest = requests.get(0);

        Assertions.assertEquals("create-auth/token-authorization", authRequest.getName());

        Assertions.assertTrue(authBody.contains("grant_type="));
        Assertions.assertTrue(authBody.contains("client_id="));
        Assertions.assertTrue(authBody.contains("client_secret="));
        Assertions.assertTrue(authBody.contains("scope="));
        Assertions.assertTrue(authBody.contains("child_key="));
        Assertions.assertTrue(authBody.contains("child_secret="));

        List<HTTPRequestEntry> createRequests = requests.subList(1, requests.size());

        Assertions.assertEquals(2, createRequests.size());

        for(HTTPRequestEntry request : createRequests) {
            Assertions.assertEquals("create-auth", request.getName());
        }
    }
}
