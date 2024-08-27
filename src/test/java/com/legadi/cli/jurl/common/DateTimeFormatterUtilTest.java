package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.DateTimeFormatterUtil.getFormatter;

import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DateTimeFormatterUtilTest {

    @Test
    public void getDefaultFormatter() {
        DateTimeFormatter formatter = Assertions.assertDoesNotThrow(
            () -> getFormatter(null));

        Assertions.assertEquals(DateTimeFormatter.ISO_LOCAL_DATE_TIME, formatter);
    }

    @Test
    public void getNamedFormatters() {
        Assertions.assertEquals(DateTimeFormatter.BASIC_ISO_DATE, getFormatter("BASIC_ISO_DATE"));
        Assertions.assertEquals(DateTimeFormatter.ISO_LOCAL_DATE, getFormatter("ISO_LOCAL_DATE"));
        Assertions.assertEquals(DateTimeFormatter.ISO_OFFSET_DATE, getFormatter("ISO_OFFSET_DATE"));
        Assertions.assertEquals(DateTimeFormatter.ISO_DATE, getFormatter("ISO_DATE"));
        Assertions.assertEquals(DateTimeFormatter.ISO_LOCAL_TIME, getFormatter("ISO_LOCAL_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_OFFSET_TIME, getFormatter("ISO_OFFSET_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_TIME, getFormatter("ISO_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_LOCAL_DATE_TIME, getFormatter("ISO_LOCAL_DATE_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_OFFSET_DATE_TIME, getFormatter("ISO_OFFSET_DATE_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_ZONED_DATE_TIME, getFormatter("ISO_ZONED_DATE_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_DATE_TIME, getFormatter("ISO_DATE_TIME"));
        Assertions.assertEquals(DateTimeFormatter.ISO_ORDINAL_DATE, getFormatter("ISO_ORDINAL_DATE"));
        Assertions.assertEquals(DateTimeFormatter.ISO_WEEK_DATE, getFormatter("ISO_WEEK_DATE"));
        Assertions.assertEquals(DateTimeFormatter.ISO_INSTANT, getFormatter("ISO_INSTANT"));
        Assertions.assertEquals(DateTimeFormatter.RFC_1123_DATE_TIME, getFormatter("RFC_1123_DATE_TIME"));
    }

    @Test
    public void getPatternFormatter() {
        String pattern = "yyyy-MM-dd";
        DateTimeFormatter formatter = Assertions.assertDoesNotThrow(
            () -> getFormatter(pattern));

        Assertions.assertNotNull(formatter);
    }
}
