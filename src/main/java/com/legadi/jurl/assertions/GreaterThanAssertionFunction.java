package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.GREATER_THAN;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public class GreaterThanAssertionFunction extends OperatorAssertionFunction {

    @Override
    public String name() {
        return GREATER_THAN.name();
    }

    @Override
    public String alias() {
        return ">";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "left", "right" };
    }

    @Override
    protected boolean test(BigDecimal arg1, BigDecimal arg2) {
        return arg1.compareTo(arg2) > 0;
    }

    @Override
    protected boolean test(LocalDateTime arg1, LocalDateTime arg2) {
        return arg1.isAfter(arg2);
    }

    @Override
    protected boolean test(String arg1, String arg2) {
        if(arg1 == null || arg2 == null) {
            return false;
        }
        return arg1.compareToIgnoreCase(arg2) > 0;
    }

}
