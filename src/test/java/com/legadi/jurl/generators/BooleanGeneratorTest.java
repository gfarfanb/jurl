package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.BOOLEAN;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class BooleanGeneratorTest extends GeneratorTest {

    public BooleanGeneratorTest() {
        super(BOOLEAN);
    }

    @Test
    public void booleanDefault() {
        String value = Assertions.assertDoesNotThrow(() -> generate());

        Assertions.assertNotNull(value);
        Assertions.assertTrue(
            Boolean.TRUE.toString().equals(value)
            || Boolean.FALSE.toString().equals(value));
    }
}
