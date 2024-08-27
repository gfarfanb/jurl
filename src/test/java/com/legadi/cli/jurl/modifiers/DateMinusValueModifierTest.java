package com.legadi.cli.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DateMinusValueModifierTest extends ValueModifierTest<DateMinusValueModifier> {

    public DateMinusValueModifierTest() {
        super("date-minus");
    }

    @Test
    public void minus() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

        String result = Assertions.assertDoesNotThrow(
            () -> apply("date-minus~ISO_LOCAL_DATE_TIME~DAYS~5", formatter.format(date)));

        Assertions.assertEquals(formatter.format(date.minus(5, ChronoUnit.DAYS)), result);
    }
}
