package com.legadi.jurl.modifiers;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.function.Function;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidModifierOperationException;

public class DateTimeValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "datetime";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "(plus|minus|epoch)", "pattern", "time-unit", "input?" };
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(args[1]);
        LocalDateTime date = LocalDateTime.from(formatter.parse(value));
        ChronoUnit timeUnit = ChronoUnit.valueOf(args[2]);
        long input = Long.parseLong(getter.apply(args[3]));

        switch(args[0].toLowerCase()) {
            case "plus":
                return formatter.format(date.plus(input, timeUnit));
            case "minus":
                return formatter.format(date.minus(input, timeUnit));
            case "epoch":
                Duration secondsDuration = Duration.ofSeconds(date.toEpochSecond(ZoneOffset.UTC));
                return Long.toString(durationToTimeUnit(secondsDuration, timeUnit));
            default:
                throw new InvalidModifierOperationException(args[0]);
        }
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
