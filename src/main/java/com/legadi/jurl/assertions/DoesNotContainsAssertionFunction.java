package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

public class DoesNotContainsAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "DOES_NOT_CONTAINS";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "search" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && !args[0].contains(args[1]);
    }

}
