package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class UUIDGeneratorTest extends GeneratorTest {

    public UUIDGeneratorTest() {
        super("UUID");
    }

    @Test
    public void uuidValidation() {
        String value = Assertions.assertDoesNotThrow(() -> generate());

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> java.util.UUID.fromString(value));
    }
}
