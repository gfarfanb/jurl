package com.legadi.cli.jurl.modifiers;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class WindowsSeparatorValueModifierTest extends ValueModifierAbstractTest<WindowsSeparatorValueModifier> {

    public WindowsSeparatorValueModifierTest() {
        super("win-separator");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[0];
    }

    @Test
    public void windowsSeparator() {
        String[] args = {};
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, ".\\executions\\src\\test"));

        Assertions.assertEquals(".\\\\executions\\\\src\\\\test", result);
    }
}
