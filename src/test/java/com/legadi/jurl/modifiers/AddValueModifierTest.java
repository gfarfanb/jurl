package com.legadi.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class AddValueModifierTest extends ValueModifierTest<AddValueModifier> {

    public AddValueModifierTest() {
        super("add");
    }

    @Test
    public void add() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("add~5", "5"));

        Assertions.assertEquals("10", result);
    }
}
