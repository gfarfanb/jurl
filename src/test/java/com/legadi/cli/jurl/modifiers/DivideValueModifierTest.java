package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DivideValueModifierTest extends ValueModifierAbstractTest<DivideValueModifier> {

    public DivideValueModifierTest() {
        super("divide");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "5" };
    }

    @Test
    public void divide() {
        String[] args = { "5" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "25"));

        Assertions.assertEquals("5", result);
    }
}
