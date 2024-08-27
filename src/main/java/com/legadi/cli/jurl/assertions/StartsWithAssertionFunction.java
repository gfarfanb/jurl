package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

public class StartsWithAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "STARTS_WITH";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "prefix" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && args[0].startsWith(args[1]);
    }

}
