package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class AddValueModifierTest extends ValueModifierAbstractTest<AddValueModifier> {

    public AddValueModifierTest() {
        super("add");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "5" };
    }

    @Test
    public void add() {
        String[] args = { "5" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "5"));

        Assertions.assertEquals("10", result);
    }
}
