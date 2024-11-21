package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class MultiplyValueModifierTest extends ValueModifierAbstractTest<MultiplyValueModifier> {

    public MultiplyValueModifierTest() {
        super("multiply");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "5" };
    }

    @Test
    public void multiply() {
        String[] args = { "5" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "5"));

        Assertions.assertEquals("25", result);
    }
}
