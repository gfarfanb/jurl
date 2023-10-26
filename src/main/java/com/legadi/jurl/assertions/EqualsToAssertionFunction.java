package com.legadi.jurl.assertions;

import java.math.BigDecimal;
import java.util.Objects;

public class EqualsToAssertionFunction extends OperatorAssertionFunction {

    @Override
    public String name() {
        return "EQUALS_TO";
    }

    @Override
    public String alias() {
        return "==";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "left", "right" };
    }

    @Override
    protected boolean test(BigDecimal arg1, BigDecimal arg2) {
        return arg1.compareTo(arg2) == 0;
    }

    @Override
    protected boolean test( String arg1, String arg2) {
        return Objects.equals(arg1, arg2);
    }
}
