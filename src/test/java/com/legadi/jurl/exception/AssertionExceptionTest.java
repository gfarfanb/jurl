package com.legadi.jurl.exception;

import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.jurl.model.AssertionType.ASSERTION;

import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.assertions.AssertionFunction;
import com.legadi.jurl.model.AssertionEntry;

public class AssertionExceptionTest {

    @Test
    public void assertionByNameException() {
        AssertionFunction function = findByNameOrFail(AssertionFunction.class, "EQUALS_TO");
        String[] values = new String[] { "5", "5" };

        AssertionEntry entry = new AssertionEntry();
        entry.setType(ASSERTION);
        entry.setName(function.name());

        try {
            throw new AssertionException(entry, function.getArgs(), values, null);
        } catch(RuntimeException ex) {
            String expected = ASSERTION.name().toLowerCase() + "=" + function.name()
                + " args=" + Arrays.toString(function.getArgs())
                + " values=" + Arrays.toString(values);

            Assertions.assertDoesNotThrow(() -> (AssertionException) ex);
            Assertions.assertEquals(expected, ex.getMessage());
        }
    }

    @Test
    public void assertionByClassException() {
        AssertionFunction function = findByNameOrFail(AssertionFunction.class, "EQUALS_TO");
        String[] values = new String[] { "5", "5" };

        AssertionEntry entry = new AssertionEntry();
        entry.setType(ASSERTION);
        entry.setName(function.name());
        entry.setAssertionClass(function.getClass().getName());

        try {
            throw new AssertionException(entry, function.getArgs(), values, "Invalid");
        } catch(AssertionException ex) {
            String expected = ASSERTION.name().toLowerCase() + "=" + function.name()
                + "(" + function.getClass().getName() + ")"
                + " args=" + Arrays.toString(function.getArgs())
                + " values=" + Arrays.toString(values)
                + " - Invalid";

            Assertions.assertDoesNotThrow(() -> (AssertionException) ex);
            Assertions.assertEquals(expected, ex.getMessage());
        }
    }
}
