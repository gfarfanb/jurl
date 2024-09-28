package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class SubtractValueModifierTest extends ValueModifierAbstractTest<SubtractValueModifier> {

    public SubtractValueModifierTest() {
        super("subtract");
    }

    @Test
    public void subtract() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("subtract~5", "10"));

        Assertions.assertEquals("5", result);
    }
}
