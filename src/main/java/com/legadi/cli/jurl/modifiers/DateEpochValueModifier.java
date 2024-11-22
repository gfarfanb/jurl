package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.DateTimeFormatterUtil.getFormatter;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.function.Function;

import com.legadi.cli.jurl.exception.CommandException;

public class DateEpochValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "date-epoch";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "pattern", "time-unit" };
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        DateTimeFormatter formatter = getFormatter(args[0]);
        LocalDateTime date = LocalDateTime.from(formatter.parse(value));
        ChronoUnit timeUnit = ChronoUnit.valueOf(args[1]);
        Duration secondsDuration = Duration.ofSeconds(date.toEpochSecond(ZoneOffset.UTC));

        return Long.toString(durationToTimeUnit(secondsDuration, timeUnit));
    }

    private long durationToTimeUnit(Duration secondsDuration, ChronoUnit timeUnit) {
        switch(timeUnit) {
            case MILLIS:
                return secondsDuration.toMillis();
            case NANOS:
                return secondsDuration.toNanos();
            case SECONDS:
                return secondsDuration.getSeconds();
            case MINUTES:
                return secondsDuration.toMinutes();
            case HOURS:
                return secondsDuration.toHours();
            case DAYS:
                return secondsDuration.toDays();
            default:
                throw new CommandException("Invalid time unit for epoch convertion: " + timeUnit);
        }
    }
}
