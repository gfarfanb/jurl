package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsNotNullAssertionFunctionTest extends AssertionFunctionTest<IsNotNullAssertionFunction> {

    public IsNotNullAssertionFunctionTest() {
        super("IS_NOT_NULL");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());
    }

    @Test
    public void isNotNull() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Is Not Null"));
    }

    @Test
    public void isNotNullFailed() {
        String[] args = new String[] { null };

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(args));
    }
}
