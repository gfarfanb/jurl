package com.legadi.jurl.modifiers;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

public class DatePlusValueModifier extends DateOperationValueModifier {

    @Override
    public String name() {
        return "date-plus";
    }

    @Override
    protected LocalDateTime apply(LocalDateTime date, long input, ChronoUnit timeUnit) {
        return date.plus(input, timeUnit);
    }
}
