package com.legadi.cli.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DatePlusValueModifierTest extends ValueModifierAbstractTest<DatePlusValueModifier> {

    public DatePlusValueModifierTest() {
        super("date-plus");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "ISO_LOCAL_DATE_TIME", "DAYS", "5" };
    }

    @Test
    public void plus() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        String[] args = { "ISO_LOCAL_DATE_TIME", "DAYS", "5" };

        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, formatter.format(date)));

        Assertions.assertEquals(formatter.format(date.plus(5, ChronoUnit.DAYS)), result);
    }
}
