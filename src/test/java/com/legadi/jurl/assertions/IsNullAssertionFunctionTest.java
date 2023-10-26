package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsNullAssertionFunctionTest extends AssertionFunctionTest<IsNullAssertionFunction> {

    public IsNullAssertionFunctionTest() {
        super("IS_NULL");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());
    }

    @Test
    public void isNull() {
        String[] args = new String[] { null };

        Assertions.assertDoesNotThrow(
            () -> evaluate(args));
    }

    @Test
    public void isNullFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Is Null"));
    }
}
