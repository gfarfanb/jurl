package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class GreaterThanAssertionFunctionTest extends AssertionFunctionTest<GreaterThanAssertionFunction> {

    public GreaterThanAssertionFunctionTest() {
        super("GREATER_THAN");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void greaterThan() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "5"));

        Assertions.assertDoesNotThrow(
                () -> evaluate("b", "a"));
    }

    @Test
    public void greaterThanFailed() {
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
