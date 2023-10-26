package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsBetweenAssertionFunctionTest extends AssertionFunctionTest<IsBetweenAssertionFunction> {

    public IsBetweenAssertionFunctionTest() {
        super("IS_BETWEEN");
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
    public void isBetween() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("5", "10", "15"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("5", "5", "15"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "15", "15"));
    }

    @Test
    public void isBetweenFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "5", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "5", "15"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "20", "15"));
    }
}
