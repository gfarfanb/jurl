package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.DATE_TIME;

import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;

public class DateTimeGeneratorTest extends GeneratorTest {

    public DateTimeGeneratorTest() {
        super(DATE_TIME);
    }

    @Test
    public void dateTimeDefault() {
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> DateTimeGenerator.DEFAULT_FORMATTER.parse(value));
    }

    @Test
    public void dateTimeFormat() {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        String value = generate("yyyy-MM-dd");

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> formatter.parse(value));
    }

    @Test
    public void dateTimeWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("year-month-day"));
    }
}
