package com.legadi.cli.jurl.modifiers;
import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.STARTS_WITH;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "date_plus")
@Evaluable(values = { "date_plus" }, op = STARTS_WITH)
public class DatePlusValueModifier extends DateOperationValueModifier {


    @Override
    protected LocalDateTime apply(LocalDateTime date, long input, ChronoUnit timeUnit) {
        return date.plus(input, timeUnit);
    }
}
