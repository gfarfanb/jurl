package com.legadi.cli.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

public class DateMinusValueModifier extends DateOperationValueModifier {

    @Override
    public String name() {
        return "date-minus";
    }

    @Override
    protected LocalDateTime apply(LocalDateTime date, long input, ChronoUnit timeUnit) {
        return date.minus(input, timeUnit);
    }
}
