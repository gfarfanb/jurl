package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class SubtractValueModifierTest extends ValueModifierAbstractTest<SubtractValueModifier> {

    public SubtractValueModifierTest() {
        super("subtract");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "5" };
    }

    @Test
    public void subtract() {
        String[] args = { "5" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "10"));

        Assertions.assertEquals("5", result);
    }
}
