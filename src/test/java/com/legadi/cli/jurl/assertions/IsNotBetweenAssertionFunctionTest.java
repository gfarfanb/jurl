package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class IsNotBetweenAssertionFunctionTest extends AssertionFunctionTest<IsNotBetweenAssertionFunction> {

    public IsNotBetweenAssertionFunctionTest() {
        super("IS_NOT_BETWEEN");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("begin"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("begin", "value"));
    }

    @Test
    public void isNotBetween() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "5", "15"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "20", "15"));
    }

    @Test
    public void isNotBetweenFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("5", null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("5", "10", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("5", "10", "15"));
    }
}
