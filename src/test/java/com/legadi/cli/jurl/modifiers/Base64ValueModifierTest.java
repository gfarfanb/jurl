package com.legadi.cli.jurl.modifiers;

import java.util.Base64;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class Base64ValueModifierTest extends ValueModifierAbstractTest<Base64ValueModifier> {

    public Base64ValueModifierTest() {
        super("base64");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "encode" };
    }

    @Test
    public void encode() {
        String[] args = { "encode" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "{\"name\": \"test\"}"));

        String encoded = new String(Base64.getEncoder().encode("{\"name\": \"test\"}".getBytes()));

        Assertions.assertEquals(encoded, result);
    }

    @Test
    public void decode() {
        String[] args = { "decode" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "eyJuYW1lIjogInRlc3QifQ=="));

        String decoded = new String(Base64.getDecoder().decode("eyJuYW1lIjogInRlc3QifQ==".getBytes()));

        Assertions.assertEquals(decoded, result);
    }

    @Test
    public void invalid() {
        String[] args = { "invalid" };
        
        Assertions.assertThrows(CommandException.class,
            () -> apply(args, "eyJuYW1lIjogInRlc3QifQ=="));
    }
}
