package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class WindowsSeparatorValueModifierTest extends ValueModifierAbstractTest<WindowsSeparatorValueModifier> {

    public WindowsSeparatorValueModifierTest() {
        super("win-separator");
    }

    @Test
    public void windowsSeparator() {
        String result = Assertions.assertDoesNotThrow(
            () -> apply("win-separator", ".\\executions\\src\\test"));

        Assertions.assertEquals(".\\\\executions\\\\src\\\\test", result);
    }
}
