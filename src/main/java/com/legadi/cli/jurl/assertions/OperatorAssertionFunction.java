package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNumeric;

import java.math.BigDecimal;
import java.util.regex.Pattern;

public abstract class OperatorAssertionFunction implements AssertionFunction {

    public final Pattern datePattern = Pattern.compile("^\\[(.*)\\]:(.*)$");

    @Override
    public boolean apply(String[] args) {
        if(isNumeric(args[0]) && isNumeric(args[1])) {
            return test(new BigDecimal(args[0]), new BigDecimal(args[1]));
        }

        return test(args[0], args[1]);
    }

    protected abstract boolean test(BigDecimal arg1, BigDecimal arg2);

    protected abstract boolean test(String arg1, String arg2);
}
