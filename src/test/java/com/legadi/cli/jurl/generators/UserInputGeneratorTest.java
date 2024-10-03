package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class UserInputGeneratorTest extends GeneratorAbstractTest {

    public UserInputGeneratorTest() {
        super("INPUT");
    }

    @Test
    public void userInputValidation() {
        String message = "Input";
        String value = Assertions.assertDoesNotThrow(() -> generate(message));

        Assertions.assertNull(value);

        settings.putUserInput(message, "Overrided Input");

        String overrided = Assertions.assertDoesNotThrow(() -> generate(message));

        Assertions.assertEquals("Overrided Input", overrided);
    }
}
