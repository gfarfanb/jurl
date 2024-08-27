package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class LessThanAssertionFunctionTest extends AssertionFunctionTest<LessThanAssertionFunction> {

    public LessThanAssertionFunctionTest() {
        super("LESS_THAN");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void lessThan() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("5", "10"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("a", "b"));
    }

    @Test
    public void lessThanFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "5"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("b", "a"));
    }
}
