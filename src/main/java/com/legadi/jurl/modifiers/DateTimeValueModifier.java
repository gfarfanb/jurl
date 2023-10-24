package com.legadi.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalUnit;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.InvalidModifierOperationException;

public class DateTimeValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "DateTime";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "(plus|minus)", "pattern", "time-unit", "input" };
    }

    @Override
    public String apply(Settings settings, String[] args, String value) throws Exception {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(args[1]);
        LocalDateTime date = LocalDateTime.from(formatter.parse(value));
        TemporalUnit timeUnit = ChronoUnit.valueOf(args[2]);
        long input = Long.parseLong(settings.getOrDefault(args[3], args[3]));

        switch(args[0].toLowerCase()) {
            case "plus":
                return formatter.format(date.plus(input, timeUnit));
            case "minus":
                return formatter.format(date.minus(input, timeUnit));
            default:
                throw new InvalidModifierOperationException(args[0]);
        }
    }
}
