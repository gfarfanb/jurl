package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class SuffixValueModifierTest extends ValueModifierAbstractTest<SuffixValueModifier> {

    public SuffixValueModifierTest() {
        super("suffix");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { ".val" };
    }

    @Test
    public void suffix() {
        String[] args = { ".val" };
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "name"));

        Assertions.assertEquals("name.val", result);
    }
}
