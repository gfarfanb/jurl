package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class IntegerGeneratorTest extends GeneratorAbstractTest {

    public IntegerGeneratorTest() {
        super("INTEGER");
    }

    @Test
    public void integerValidation() {
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
