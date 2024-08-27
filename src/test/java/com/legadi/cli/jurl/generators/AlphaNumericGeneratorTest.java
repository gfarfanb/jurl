package com.legadi.cli.jurl.generators;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class AlphaNumericGeneratorTest extends GeneratorTest {

    public AlphaNumericGeneratorTest() {
        super("ALPHA_NUMERIC");
    }

    @Test
    public void alphaNumericValidation() {
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertTrue(value.length() >= AlphaNumericGenerator.RANGE_LENGTH);
    }

    @Test
    public void alphaNumericLength() {
        String value = generate("25");

        Assertions.assertNotNull(value);
        Assertions.assertEquals(25, value.length());
    }

    @Test
    public void alphaNumericWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("size"));
    }
}
