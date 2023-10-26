package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

public class IsNotBlankAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_NOT_BLANK";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0]);
    }

}
