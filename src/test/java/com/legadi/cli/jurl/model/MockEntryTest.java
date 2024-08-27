package com.legadi.cli.jurl.model;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class MockEntryTest {

    @Test
    public void setterGetterValidation() {
        MockEntry model = new MockEntry();

        model.setStatusCode("200");
        model.setSecondsDelay("5");

        Assertions.assertEquals("200", model.getStatusCode());
        Assertions.assertEquals("5", model.getSecondsDelay());
    }
}
