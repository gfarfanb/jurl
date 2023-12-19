package com.legadi.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DatePlusValueModifierTest extends ValueModifierTest<DatePlusValueModifier> {

    public DatePlusValueModifierTest() {
        super("date-plus");
    }

    @Test
    public void plus() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

        String result = Assertions.assertDoesNotThrow(
            () -> apply("date-plus~ISO_LOCAL_DATE_TIME~DAYS~5", formatter.format(date)));

        Assertions.assertEquals(formatter.format(date.plus(5, ChronoUnit.DAYS)), result);
    }
}
