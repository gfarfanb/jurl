package com.legadi.jurl.model;

import static com.legadi.jurl.model.AuthorizationType.TOKEN;
import static com.legadi.jurl.model.AuthorizationType.BASIC;
import static com.legadi.jurl.model.AuthorizationType.EMPTY;
import static com.legadi.jurl.model.AuthorizationType.valueOfOrDefault;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class AuthorizationTypeTest {

    @Test
    public void valueOfValidation() {
        Assertions.assertEquals(BASIC, valueOfOrDefault("BASIC"));
        Assertions.assertEquals(TOKEN, valueOfOrDefault("TOKEN"));
        Assertions.assertEquals(EMPTY, valueOfOrDefault("EMPTY"));
        Assertions.assertEquals(EMPTY, valueOfOrDefault("UNKNOWN"));
    }
}
