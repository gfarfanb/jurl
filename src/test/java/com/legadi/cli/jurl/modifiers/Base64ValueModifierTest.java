package com.legadi.cli.jurl.modifiers;

import java.util.Base64;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class Base64ValueModifierTest extends ValueModifierAbstractTest<Base64ValueModifier> {

    public Base64ValueModifierTest() {
        super("base64");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[0];
    }

    @Test
    public void encode() {
        String[] args = {};
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "{\"name\": \"test\"}"));

        String encoded = new String(Base64.getEncoder().encode("{\"name\": \"test\"}".getBytes()));

        Assertions.assertEquals(encoded, result);
    }
}
