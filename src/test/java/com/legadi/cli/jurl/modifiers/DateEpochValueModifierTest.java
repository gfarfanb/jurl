package com.legadi.cli.jurl.modifiers;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.CommandException;

public class DateEpochValueModifierTest extends ValueModifierAbstractTest<DateEpochValueModifier> {

    public DateEpochValueModifierTest() {
        super("date-epoch");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[] { "ISO_LOCAL_DATE_TIME", "MILLIS" };
    }

    @Test
    public void epoch() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        Duration secondsDuration = Duration.ofSeconds(date.toEpochSecond(ZoneOffset.UTC));

        String[] epochMillisArgs = { "ISO_LOCAL_DATE_TIME", "MILLIS" };
        String epochMillis = Assertions.assertDoesNotThrow(
            () -> apply(epochMillisArgs, formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toMillis()), epochMillis);

        String[] epochNanosArgs = { "ISO_LOCAL_DATE_TIME", "NANOS" };
        String epochNanos = Assertions.assertDoesNotThrow(
            () -> apply(epochNanosArgs, formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toNanos()), epochNanos);

        String[] epochSecondsArgs = { "ISO_LOCAL_DATE_TIME", "SECONDS" };
        String epochSeconds = Assertions.assertDoesNotThrow(
            () -> apply(epochSecondsArgs, formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.getSeconds()), epochSeconds);

        String[] epochMinutesArgs = { "ISO_LOCAL_DATE_TIME", "MINUTES" };
        String epochMinutes = Assertions.assertDoesNotThrow(
            () -> apply(epochMinutesArgs, formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toMinutes()), epochMinutes);

        String[] epochHoursArgs = { "ISO_LOCAL_DATE_TIME", "HOURS" };
        String epochHours = Assertions.assertDoesNotThrow(
            () -> apply(epochHoursArgs, formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toHours()), epochHours);

        String[] epochDaysArgs = { "ISO_LOCAL_DATE_TIME", "DAYS" };
        String epochDays = Assertions.assertDoesNotThrow(
            () -> apply(epochDaysArgs, formatter.format(date)));
        Assertions.assertEquals(Long.toString(secondsDuration.toDays()), epochDays);
    }

    @Test
    public void epochInvalid() {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME;
        String[] args = { "ISO_LOCAL_DATE_TIME", "MONTHS" };

        Assertions.assertThrows(CommandException.class,
            () -> apply(args, formatter.format(date)));
    }
}
