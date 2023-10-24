package com.legadi.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PasswordInputGeneratorTest extends GeneratorTest {

    public PasswordInputGeneratorTest() {
        super("PASSWORD");
    }

    @Test
    public void passwordInputValidation() {
        String message = "P4s5w0rd";
        String value = Assertions.assertDoesNotThrow(() -> generate(message));

        Assertions.assertNotNull(value);
        Assertions.assertEquals("P4s5w0rd", value);
        Assertions.assertEquals("P4s5w0rd", settings.get(message));

        settings.putOverride(message, "0v3rrid3d_P4s5w0rd");

        String overrided = Assertions.assertDoesNotThrow(() -> generate(message));

        Assertions.assertEquals("0v3rrid3d_P4s5w0rd", overrided);
    }
}
