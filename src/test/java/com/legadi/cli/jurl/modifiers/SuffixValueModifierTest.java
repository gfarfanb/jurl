package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class SuffixValueModifierTest extends ValueModifierTest<SuffixValueModifier> {

    public SuffixValueModifierTest() {
        super("suffix");
    }

    @Test
    public void suffix() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("suffix~.val", "name"));

        Assertions.assertEquals("name.val", result);
    }
}
