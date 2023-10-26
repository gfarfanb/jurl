package com.legadi.jurl.assertions;

import java.math.BigDecimal;

public class GreaterThanAssertionFunction extends OperatorAssertionFunction {

    @Override
    public String name() {
        return "GREATER_THAN";
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
    protected boolean test(String arg1, String arg2) {
        if(arg1 == null || arg2 == null) {
            return false;
        }
        return arg1.compareToIgnoreCase(arg2) > 0;
    }

}
