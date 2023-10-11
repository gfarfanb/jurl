package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.LESS_THAN_OR_EQUALS_TO;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class LessThanOrEqualsToAssertionFunctionTest extends AssertionFunctionTest<LessThanOrEqualsToAssertionFunction> {

    public LessThanOrEqualsToAssertionFunctionTest() {
        super(LESS_THAN_OR_EQUALS_TO);
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void lessThanOrEqualsTo() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("5", "10"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("10", "10"));
    }

    @Test
    public void lessThanOrEqualsToFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("10", "5"));
    }
}