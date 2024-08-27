package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import java.math.BigDecimal;

public class IsBetweenAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_BETWEEN";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "begin", "value", "end" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && isNotBlank(args[2])
            && new BigDecimal(args[1]).compareTo(new BigDecimal(args[0])) >= 0
            && new BigDecimal(args[1]).compareTo(new BigDecimal(args[2])) <= 0;
    }

}
