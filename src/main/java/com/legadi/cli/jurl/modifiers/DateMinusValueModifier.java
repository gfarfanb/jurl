package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "date_minus")
@Evaluable(values = { "date_minus" }, op = STARTS_WITH)
public class DateMinusValueModifier extends DateOperationValueModifier {

    @Override
    protected LocalDateTime apply(LocalDateTime date, long input, ChronoUnit timeUnit) {
        return date.minus(input, timeUnit);
    }
}
