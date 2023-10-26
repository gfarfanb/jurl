package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsNotBlankAssertionFunctionTest extends AssertionFunctionTest<IsNotBlankAssertionFunction> {

    public IsNotBlankAssertionFunctionTest() {
        super("IS_NOT_BLANK");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());
    }

    @Test
    public void isNotBlank() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Is Not Blank"));
    }

    @Test
    public void isNotBlankFailed() {
        String[] args = new String[] { null };

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(args));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(""));
    }
}
