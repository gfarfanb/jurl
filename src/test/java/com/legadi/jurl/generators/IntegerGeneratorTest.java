package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.INTEGER;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;

public class IntegerGeneratorTest extends GeneratorTest {

    public IntegerGeneratorTest() {
        super(INTEGER);
    }

    @Test
    public void integerDefault() {
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> Integer.parseInt(value));
        Assertions.assertEquals(IntegerGenerator.DEFAULT_LENGTH, value.length());
    }

    @Test
    public void integerLength() {
        String value = generate("10");

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> Long.parseLong(value));
        Assertions.assertEquals(10, value.length());
    }

    @Test
    public void integerWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("length"));
    }
}
