package com.legadi.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DefaultValueModifierTest extends ValueModifierTest<DefaultValueModifier> {

    public DefaultValueModifierTest() {
        super("default");
    }

    @Test
    public void originalValue() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("default~5", "10"));

        Assertions.assertEquals("10", result);
    }

    @Test
    public void defaultValue() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("default~5", null));

        Assertions.assertEquals("5", result);
    }
}
