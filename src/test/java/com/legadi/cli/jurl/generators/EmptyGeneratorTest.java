package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class EmptyGeneratorTest extends GeneratorAbstractTest {

    public EmptyGeneratorTest() {
        super("EMPTY");
    }

    @Test
    public void emptyValidation() {
        String value = Assertions.assertDoesNotThrow(() -> generate());

        Assertions.assertNotNull(value);
        Assertions.assertTrue(value.isEmpty());
    }
}
