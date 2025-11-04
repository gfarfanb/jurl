package com.legadi.cli.jurl.exception;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;

import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.modifiers.ValueModifier;

public class ModifierExceptionTest {

    @Test
    public void modifierException() {
        ValueModifier modifier = findOrFail(ValueModifier.class, "add");
        String[] values = new String[] { "5" };

        try {
            throw new ModifierException(extractNamedName(modifier), modifier.getArgs(), values, "add:5", null);
        } catch(RuntimeException ex) {
            String expected = "modifier=" + extractNamedName(modifier)
                + " args=" + Arrays.toString(modifier.getArgs())
                + " values=" + Arrays.toString(values)
                + " input=add:5";

            Assertions.assertDoesNotThrow(() -> (ModifierException) ex);
            Assertions.assertEquals(expected, ex.getMessage());
        }
    }

    @Test
    public void modifierExceptionMessage() {
        ValueModifier modifier = findOrFail(ValueModifier.class, "add");
        String[] values = new String[] { "5" };

        try {
            throw new ModifierException(extractNamedName(modifier), modifier.getArgs(), values, "add:5", "Invalid arguments");
        } catch(RuntimeException ex) {
            String expected = "modifier=" + extractNamedName(modifier)
                + " args=" + Arrays.toString(modifier.getArgs())
                + " values=" + Arrays.toString(values)
                + " input=add:5 - Invalid arguments";

            Assertions.assertDoesNotThrow(() -> (ModifierException) ex);
            Assertions.assertEquals(expected, ex.getMessage());
        }
    }
}
