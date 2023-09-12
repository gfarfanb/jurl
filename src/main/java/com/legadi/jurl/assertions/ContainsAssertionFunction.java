package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.AssertionType.CONTAINS;

public class ContainsAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return CONTAINS.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "search" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && args[0].contains(args[1]);
    }

}
