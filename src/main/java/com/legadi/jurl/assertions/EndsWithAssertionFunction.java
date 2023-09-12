package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.END_WITH;

public class EndsWithAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return END_WITH.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "prefix" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && args[0].endsWith(args[1]);
    }

}
