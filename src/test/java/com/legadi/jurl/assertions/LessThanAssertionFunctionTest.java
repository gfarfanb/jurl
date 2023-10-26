package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

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
    }

    @Test
    public void lessThanFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "5"));
    }
}
