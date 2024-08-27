package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class EqualsToAssertionFunctionTest extends AssertionFunctionTest<EqualsToAssertionFunction> {

    public EqualsToAssertionFunctionTest() {
        super("EQUALS_TO");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void equalsTo() {
        Assertions.assertDoesNotThrow(
            () -> evaluate(null, null));

        Assertions.assertDoesNotThrow(
            () -> evaluate("Equals to", "Equals to"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "10"));
    }

    @Test
    public void equalsToFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Equals to", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Equals to", "Equals To"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("5", "10"));
    }
}
