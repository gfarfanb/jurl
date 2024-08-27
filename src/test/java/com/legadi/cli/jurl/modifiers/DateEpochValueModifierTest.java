package com.legadi.cli.jurl.modifiers;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class DateEpochValueModifierTest extends ValueModifierTest<DateEpochValueModifier> {

    public DateEpochValueModifierTest() {
        super("date-epoch");
    }

    @Test
    public void epoch() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        Duration secondsDuration = Duration.ofSeconds(date.toEpochSecond(ZoneOffset.UTC));

        String epochMillis = Assertions.assertDoesNotThrow(
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~MILLIS", formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toMillis()), epochMillis);

        String epochNanos = Assertions.assertDoesNotThrow(
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~NANOS", formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toNanos()), epochNanos);

        String epochSeconds = Assertions.assertDoesNotThrow(
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~SECONDS", formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.getSeconds()), epochSeconds);

        String epochMinutes = Assertions.assertDoesNotThrow(
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~MINUTES", formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toMinutes()), epochMinutes);

        String epochHours = Assertions.assertDoesNotThrow(
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~HOURS", formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toHours()), epochHours);

        String epochDays = Assertions.assertDoesNotThrow(
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~DAYS", formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toDays()), epochDays);
    }

    @Test
    public void epochInvalid() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

        Assertions.assertThrows(CommandException.class,
            () -> apply("date-epoch~ISO_LOCAL_DATE_TIME~MONTHS", formatter.format(date)));
    }
}
