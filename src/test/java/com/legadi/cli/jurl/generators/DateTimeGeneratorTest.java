package com.legadi.cli.jurl.generators;

import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.DateTimeFormatterUtil;
import com.legadi.cli.jurl.exception.CommandException;

public class DateTimeGeneratorTest extends GeneratorAbstractTest {

    public DateTimeGeneratorTest() {
        super("DATE-TIME");
    }

    @Test
    public void dateTimeValidation() {
        String value = generate();

        Assertions.assertNotNull(value);
        Assertions.assertDoesNotThrow(() -> DateTimeFormatterUtil.DEFAULT_FORMATTER.parse(value));
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
