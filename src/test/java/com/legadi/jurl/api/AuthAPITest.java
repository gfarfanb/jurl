package com.legadi.jurl.api;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.embedded.EmbeddedAPITest;
import com.legadi.jurl.model.http.HTTPRequestEntry;

public class AuthAPITest extends EmbeddedAPITest {

    @Test
    public void requestWithAuthTimes() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-t", "5",
                "-n", "create",
                "src/test/resources/auth-request.spec.http"
            ));

        List<HTTPRequestEntry> requests = requestCatcher.getAll(correlationId, "request");

        Assertions.assertEquals(6, requests.size());
    }
}
