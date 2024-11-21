package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PrefixValueModifierTest extends ValueModifierAbstractTest<PrefixValueModifier> {

    public PrefixValueModifierTest() {
        super("prefix");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "prop." };
    }

    @Test
    public void prefix() {
        String[] args = { "prop." };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "name"));

        Assertions.assertEquals("prop.name", result);
    }
}
