package com.legadi.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class UserInputGeneratorTest extends GeneratorTest {

    public UserInputGeneratorTest() {
        super("INPUT");
    }

    @Test
    public void userInputValidation() {
        String message = "Input";
        String value = Assertions.assertDoesNotThrow(() -> generate(message));

        Assertions.assertNotNull(value);
        Assertions.assertEquals("Input", value);
        Assertions.assertEquals("Input", settings.get(message));

        settings.putOverride(message, "Overrided Input");

        String overrided = Assertions.assertDoesNotThrow(() -> generate(message));

        Assertions.assertEquals("Overrided Input", overrided);
    }
}
