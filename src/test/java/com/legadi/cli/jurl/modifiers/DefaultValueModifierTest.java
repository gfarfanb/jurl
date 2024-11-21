package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DefaultValueModifierTest extends ValueModifierAbstractTest<DefaultValueModifier> {

    public DefaultValueModifierTest() {
        super("default");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "5" };
    }

    @Test
    public void originalValue() {
        String[] args = { "5" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "10"));

        Assertions.assertEquals("10", result);
    }

    @Test
    public void defaultValue() {
        String[] args = { "5" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, null));

        Assertions.assertEquals("5", result);
    }
}
