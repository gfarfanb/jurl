package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class StartsWithAssertionFunctionTest extends AssertionFunctionTest<StartsWithAssertionFunction> {

    public StartsWithAssertionFunctionTest() {
        super("STARTS_WITH");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("value"));
    }

    @Test
    public void startsWith() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Starts With", "Starts"));
    }

    @Test
    public void startsWithFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Starts With", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Starts With", "starts"));
    }
}
