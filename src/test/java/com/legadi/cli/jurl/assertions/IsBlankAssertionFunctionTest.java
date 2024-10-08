package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class IsBlankAssertionFunctionTest extends AssertionFunctionTest<IsBlankAssertionFunction> {

    public IsBlankAssertionFunctionTest() {
        super("IS_BLANK");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());
    }

    @Test
    public void isBlank() {
        String[] args = new String[] { null };

        Assertions.assertDoesNotThrow(
            () -> evaluate(args));

        Assertions.assertDoesNotThrow(
            () -> evaluate(""));
    }

    @Test
    public void isBlankFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Is Blank"));
    }
}
