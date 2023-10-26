package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsInAssertionFunctionTest extends AssertionFunctionTest<IsInAssertionFunction> {

    public IsInAssertionFunctionTest() {
        super("IS_IN");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("search"));
    }

    @Test
    public void isIn() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("5", "1", "2", "3", "4", "5"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("A", "A", "B", "C", "D", "E"));
    }

    @Test
    public void isInFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("0", "1", "2", "3", "4", "5"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Z", "A", "B", "C", "D", "E"));
    }
}
