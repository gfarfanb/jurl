package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.CONTAINS;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class ContainsAssertionFunctionTest extends AssertionFunctionTest<ContainsAssertionFunction> {

    public ContainsAssertionFunctionTest() {
        super(CONTAINS);
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("value"));
    }

    @Test
    public void contains() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Contains", "tain"));
    }

    @Test
    public void containsFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Contains", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Contains", "Tain"));
    }
}
