package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class GreaterThanOrEqualsToAssertionFunctionTest extends AssertionFunctionTest<GreaterThanOrEqualsToAssertionFunction> {

    public GreaterThanOrEqualsToAssertionFunctionTest() {
        super("GREATER_THAN_OR_EQUALS_TO");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void greaterThanOrEqualsTo() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "5"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "10"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("b", "a"));

            Assertions.assertDoesNotThrow(
            () -> evaluate("a", "a"));
    }

    @Test
    public void greaterThanOrEqualsToFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "15"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("a", "b"));
    }
}
