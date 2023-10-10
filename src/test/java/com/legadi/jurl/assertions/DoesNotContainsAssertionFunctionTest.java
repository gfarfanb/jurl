package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.DOES_NOT_CONTAINS;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class DoesNotContainsAssertionFunctionTest extends AssertionFunctionTest<DoesNotContainsAssertionFunction> {

    public DoesNotContainsAssertionFunctionTest() {
        super(DOES_NOT_CONTAINS);
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("value"));
    }

    @Test
    public void doesNotContains() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Does Not Contains", "not"));
    }

    @Test
    public void doesNotContainsFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Does Not Contains", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Does Not Contains", "Not"));
    }
}
