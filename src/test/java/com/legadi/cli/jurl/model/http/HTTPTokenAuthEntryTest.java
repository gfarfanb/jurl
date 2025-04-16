package com.legadi.cli.jurl.model.http;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.model.http.auth.HTTPTokenAuthEntry;

public class HTTPTokenAuthEntryTest {

    @Test
    public void setterGetterValidation() {
        HTTPTokenAuthEntry model = new HTTPTokenAuthEntry();
        String secret = UUID.randomUUID().toString();

        model.setTokenUrl("http://localhost:555555/oauth/token");
        model.setGrantType("client_credentials");
        model.setClientId("client-id");
        model.setClientSecret(secret);
        model.setScope("test");

        Assertions.assertEquals("http://localhost:555555/oauth/token", model.getTokenUrl());
        Assertions.assertEquals("client_credentials", model.getGrantType());
        Assertions.assertEquals("client-id", model.getClientId());
        Assertions.assertEquals(secret, model.getClientSecret());
        Assertions.assertEquals("test", model.getScope());
    }
}
