package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class BooleanGeneratorTest extends GeneratorAbstractTest {

    public BooleanGeneratorTest() {
        super("BOOLEAN");
    }

    @Test
    public void booleanValidation() {
        String value = Assertions.assertDoesNotThrow(() -> generate());

        Assertions.assertNotNull(value);
        Assertions.assertTrue(
            Boolean.TRUE.toString().equals(value)
            || Boolean.FALSE.toString().equals(value));
    }
}
