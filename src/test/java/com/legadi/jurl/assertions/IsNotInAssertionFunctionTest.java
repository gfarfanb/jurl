package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.IS_NOT_IN;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class IsNotInAssertionFunctionTest extends AssertionFunctionTest<IsNotInAssertionFunction> {

    public IsNotInAssertionFunctionTest() {
        super(IS_NOT_IN);
    }

    @Test
    public void isNotIn() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("0", "1", "2", "3", "4", "5"));

        Assertions.assertDoesNotThrow(
            () -> evaluate("Z", "A", "B", "C", "D", "E"));
    }

    @Test
    public void isNotInFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("5", "1", "2", "3", "4", "5"));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("A", "A", "B", "C", "D", "E"));
    }
}
