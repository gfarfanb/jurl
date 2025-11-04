package com.legadi.cli.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DateMinusValueModifierTest extends ValueModifierAbstractTest<DateMinusValueModifier> {

    public DateMinusValueModifierTest() {
        super("date_minus");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "ISO_LOCAL_DATE_TIME", "DAYS", "5" };
    }

    @Test
    public void minus() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        String[] args = { "ISO_LOCAL_DATE_TIME", "DAYS", "5" };

        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, formatter.format(date)));

        Assertions.assertEquals(formatter.format(date.minus(5, ChronoUnit.DAYS)), result);
    }
}
