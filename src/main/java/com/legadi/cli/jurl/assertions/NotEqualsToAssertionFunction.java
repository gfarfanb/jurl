package com.legadi.cli.jurl.assertions;

import java.math.BigDecimal;
import java.util.Objects;

public class NotEqualsToAssertionFunction extends OperatorAssertionFunction {

    @Override
    public String name() {
        return "NOT_EQUALS_TO";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "left", "right" };
    }

    @Override
    protected boolean test(BigDecimal arg1, BigDecimal arg2) {
        return arg1.compareTo(arg2) != 0;
    }

    @Override
    protected boolean test(String arg1, String arg2) {
        return !Objects.equals(arg1, arg2);
    }
}
