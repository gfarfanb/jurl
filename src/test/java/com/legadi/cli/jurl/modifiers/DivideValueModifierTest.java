package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DivideValueModifierTest extends ValueModifierTest<DivideValueModifier> {

    public DivideValueModifierTest() {
        super("divide");
    }

    @Test
    public void divide() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("divide~5", "25"));

        Assertions.assertEquals("5", result);
    }
}
