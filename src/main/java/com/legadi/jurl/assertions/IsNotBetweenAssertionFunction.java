package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.IS_NOT_BETWEEN;

import java.math.BigDecimal;

public class IsNotBetweenAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_NOT_BETWEEN.name();
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
            && !(
                new BigDecimal(args[1]).compareTo(new BigDecimal(args[0])) >= 0
                && new BigDecimal(args[1]).compareTo(new BigDecimal(args[2])) <= 0
            );
    }

}
