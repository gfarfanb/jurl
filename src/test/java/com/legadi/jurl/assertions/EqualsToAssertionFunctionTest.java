package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.EQUALS_TO;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class EqualsToAssertionFunctionTest extends AssertionFunctionTest<EqualsToAssertionFunction> {

    public EqualsToAssertionFunctionTest() {
        super(EQUALS_TO);
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("left"));
    }

    @Test
    public void equalsTo() {
        Assertions.assertDoesNotThrow(
            () -> evaluate(null, null));

        Assertions.assertDoesNotThrow(
            () -> evaluate("Equals to", "Equals to"));
    }

    @Test
    public void equalsToFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Equals to", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Equals to", "Equals To"));
    }
}
