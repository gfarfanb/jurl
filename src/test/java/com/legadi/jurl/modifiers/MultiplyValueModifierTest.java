package com.legadi.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class MultiplyValueModifierTest extends ValueModifierTest<MultiplyValueModifier> {

    public MultiplyValueModifierTest() {
        super("multiply");
    }

    @Test
    public void multiply() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("multiply~5", "5"));

        Assertions.assertEquals("25", result);
    }
}
