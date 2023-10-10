package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.END_WITH;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;

public class EndsWithAssertionFunctionTest extends AssertionFunctionTest<EndsWithAssertionFunction> {

    public EndsWithAssertionFunctionTest() {
        super(END_WITH);
    }

    @Test
    public void expectedArguments() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate());

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("value"));
    }

    @Test
    public void endsWith() {
        Assertions.assertDoesNotThrow(
            () -> evaluate("Ends With", "With"));
    }

    @Test
    public void endsWithFailed() {
        Assertions.assertThrows(AssertionException.class,
            () -> evaluate(null, null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Ends With", null));

        Assertions.assertThrows(AssertionException.class,
            () -> evaluate("Ends With", "with"));
    }
}
