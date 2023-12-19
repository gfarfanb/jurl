package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class NotEqualsToAssertionFunctionTest extends AssertionFunctionTest<NotEqualsToAssertionFunction> {

    public NotEqualsToAssertionFunctionTest() {
        super("NOT_EQUALS_TO");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void notEqualsTo() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Equals to", null));

        Assertions.assertDoesNotThrow(
            () -> evaluate("Equals to", "Equals To"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("5", "10"));
    }

    @Test
    public void notEqualsToFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Equals to", "Equals to"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "10"));
    }
}
