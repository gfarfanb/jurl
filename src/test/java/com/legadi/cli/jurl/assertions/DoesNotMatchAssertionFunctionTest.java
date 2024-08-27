package com.legadi.cli.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.AssertionException;

public class DoesNotMatchAssertionFunctionTest extends AssertionFunctionTest<DoesNotMatchAssertionFunction> {

    public DoesNotMatchAssertionFunctionTest() {
        super("DOES_NOT_MATCH");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("regex"));
    }

    @Test
    public void doesNotMatch() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Does|Not|Match", "Any"));
    }

    @Test
    public void doesNotMatchFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Does|Not|Match", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Does|Not|Match", "Match"));
    }
}
