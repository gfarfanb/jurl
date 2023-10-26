package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsFalseAssertionFunctionTest extends AssertionFunctionTest<IsFalseAssertionFunction> {

    public IsFalseAssertionFunctionTest() {
        super("IS_FALSE");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());
    }

    @Test
    public void isFalse() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("false"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("not"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("0"));
    }

    @Test
    public void isFalseFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("true"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("TRUE"));
    }
}
