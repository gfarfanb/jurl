package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.DateTimeFormatterUtil.getFormatter;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.function.Function;

public abstract class DateOperationValueModifier implements ValueModifier {

    @Override
    public String[] getArgs() {
        return new String[] { "pattern", "time-unit", "input" };
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        DateTimeFormatter formatter = getFormatter(args[0]);
        LocalDateTime date = LocalDateTime.from(formatter.parse(value));
        ChronoUnit timeUnit = ChronoUnit.valueOf(args[1]);
        long input = Long.parseLong(getter.apply(args[2]));

        return formatter.format(apply(date, input, timeUnit));
    }

    protected abstract LocalDateTime apply(LocalDateTime date, long input, ChronoUnit timeUnit);
}
