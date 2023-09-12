package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.GREATER_THAN;

import java.math.BigDecimal;

public class GreaterThanAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return GREATER_THAN.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "left", "right" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && new BigDecimal(args[0]).compareTo(new BigDecimal(args[1])) > 0;
    }

}
