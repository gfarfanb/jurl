package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PrefixValueModifierTest extends ValueModifierAbstractTest<PrefixValueModifier> {

    public PrefixValueModifierTest() {
        super("prefix");
    }

    @Test
    public void prefix() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("prefix~prop.", "name"));

        Assertions.assertEquals("prop.name", result);
    }
}
