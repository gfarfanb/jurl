package com.legadi.cli.jurl.generators;

import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.DateTimeFormatterUtil;
import com.legadi.cli.jurl.exception.CommandException;

public class DateTimeGeneratorTest extends GeneratorAbstractTest {

    public DateTimeGeneratorTest() {
        super("DATE_TIME");
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
    public void dateTimeFormatFromConstants() {
        Assertions.assertNotNull(generate("BASIC_ISO_DATE"));
        Assertions.assertNotNull(generate("ISO_LOCAL_DATE"));
        Assertions.assertNotNull(generate("ISO_OFFSET_DATE"));
        Assertions.assertNotNull(generate("ISO_DATE"));
        Assertions.assertNotNull(generate("ISO_LOCAL_TIME"));
        Assertions.assertNotNull(generate("ISO_OFFSET_TIME"));
        Assertions.assertNotNull(generate("ISO_TIME"));
        Assertions.assertNotNull(generate("ISO_LOCAL_DATE_TIME"));
        Assertions.assertNotNull(generate("ISO_OFFSET_DATE_TIME"));
        Assertions.assertNotNull(generate("ISO_ZONED_DATE_TIME"));
        Assertions.assertNotNull(generate("ISO_DATE_TIME"));
        Assertions.assertNotNull(generate("ISO_ORDINAL_DATE"));
        Assertions.assertNotNull(generate("ISO_WEEK_DATE"));
        Assertions.assertNotNull(generate("ISO_INSTANT"));
        Assertions.assertNotNull(generate("RFC_1123_DATE_TIME"));
    }

    @Test
    public void dateTimeWrongArg() {
        Assertions.assertThrows(CommandException.class,
            () -> generate("year-month-day"));
    }
}
