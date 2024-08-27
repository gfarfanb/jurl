package com.legadi.cli.jurl.model.http;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HTTPRequestAuthEntryTest {

    @Test
    public void setterGetterValidation() {
        HTTPRequestAuthEntry model = new HTTPRequestAuthEntry();

        model.setRequestInputPath("src/test/resources/auth.spec.http");
        model.setInputName("authorization");
        model.setAuthType("TOKEN");
        model.setTokenParam("auth.bearer.token");
        model.setUsernameParam("auth.basic.username");
        model.setPasswordParam("auth.basic.password");

        Assertions.assertEquals("src/test/resources/auth.spec.http", model.getRequestInputPath());
        Assertions.assertEquals("authorization", model.getInputName());
        Assertions.assertEquals("TOKEN", model.getAuthType());
        Assertions.assertEquals("auth.bearer.token", model.getTokenParam());
        Assertions.assertEquals("auth.basic.username", model.getUsernameParam());
        Assertions.assertEquals("auth.basic.password", model.getPasswordParam());
    }
}
