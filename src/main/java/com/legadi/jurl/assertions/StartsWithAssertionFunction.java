package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.STARTS_WITH;

import com.legadi.jurl.exception.AssertionException;

public class StartsWithAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return STARTS_WITH.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "prefix" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && args[0].startsWith(args[1]);
    }

}
