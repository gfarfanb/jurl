package com.legadi.jurl.assertions;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class MatchesAssertionFunctionTest extends AssertionFunctionTest<MatchesAssertionFunction> {

    public MatchesAssertionFunctionTest() {
        super("MATCHES");
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("regex"));
    }

    @Test
    public void matches() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Matches|Find", "Find"));
    }

    @Test
    public void matchesFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Matches|Find", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Matches|Find", "Not"));
    }
}
