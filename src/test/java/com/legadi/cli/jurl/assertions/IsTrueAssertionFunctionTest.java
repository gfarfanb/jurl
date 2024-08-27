package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class IsTrueAssertionFunctionTest extends AssertionFunctionTest<IsTrueAssertionFunction> {

    public IsTrueAssertionFunctionTest() {
        super("IS_TRUE");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());
    }

    @Test
    public void isTrue() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("true"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("TRUE"));
    }

    @Test
    public void isTrueFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("false"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("not"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("0"));
    }
}
