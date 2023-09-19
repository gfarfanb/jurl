package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.DOES_NOT_CONTAINS;

import com.legadi.jurl.exception.AssertionException;

public class DoesNotContainsAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return DOES_NOT_CONTAINS.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "search" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && !args[0].contains(args[1]);
    }

}
