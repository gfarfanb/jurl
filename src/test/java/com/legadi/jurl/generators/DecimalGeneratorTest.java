package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.DECIMAL;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;

public class DecimalGeneratorTest extends GeneratorTest {

    private static final int DOT_LENGTH = 1;

    public DecimalGeneratorTest() {
        super(DECIMAL);
    }

    @Test
    public void decimalDefault() {
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> Double.parseDouble(value));
        Assertions.assertEquals(DecimalGenerator.DEFAULT_LENGTH + DOT_LENGTH + DecimalGenerator.DEFAULT_DECIMAL_LENGTH,
            value.length());
    }

    @Test
    public void decimalLength() {
        String valueNumberDecimal = generate("9,3");

        Assertions.assertNotNull(valueNumberDecimal);
        Assertions.assertDoesNotThrow(() -> Double.parseDouble(valueNumberDecimal));
        Assertions.assertEquals(13, valueNumberDecimal.length());

        String valueNumber = generate("10");

        Assertions.assertNotNull(valueNumber);
        Assertions.assertDoesNotThrow(() -> Double.parseDouble(valueNumber));
        Assertions.assertEquals(10 + DOT_LENGTH + DecimalGenerator.DEFAULT_DECIMAL_LENGTH,
            valueNumber.length());
    }

    @Test
    public void decimalWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("precision"));
    }
}
