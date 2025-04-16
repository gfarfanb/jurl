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
        model.getHeaders().put("Content-Type", "application/x-www-form-urlencoded");
        model.setGrantType("client_credentials");
        model.setGrantTypeFieldName("grant_type");
        model.setClientId("client-id");
        model.setClientIdFieldName("client_id");
        model.setClientSecret(secret);
        model.setClientSecretFieldName("client_secret");
        model.setScope("test");
        model.setScopeFieldName("scope");
        model.setAccessTokenFieldName("access_token");
        model.setExpiresInFieldName("expires_in");
        model.setExpiresInTimeUnit("SECONDS");
        model.setTokenTypeFieldName("token_type");

        Assertions.assertEquals("http://localhost:555555/oauth/token", model.getTokenUrl());
        Assertions.assertEquals("application/x-www-form-urlencoded", model.getHeaders().get("Content-Type"));
        Assertions.assertEquals("client_credentials", model.getGrantType());
        Assertions.assertEquals("grant_type", model.getGrantTypeFieldName());
        Assertions.assertEquals("client-id", model.getClientId());
        Assertions.assertEquals("client_id", model.getClientIdFieldName());
        Assertions.assertEquals(secret, model.getClientSecret());
        Assertions.assertEquals("client_secret", model.getClientSecretFieldName());
        Assertions.assertEquals("test", model.getScope());
        Assertions.assertEquals("scope", model.getScopeFieldName());
        Assertions.assertEquals("access_token", model.getAccessTokenFieldName());
        Assertions.assertEquals("expires_in", model.getExpiresInFieldName());
        Assertions.assertEquals("SECONDS", model.getExpiresInTimeUnit());
        Assertions.assertEquals("token_type", model.getTokenTypeFieldName());
    }
}
