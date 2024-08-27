package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

public class EndsWithAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "END_WITH";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value", "sufix" };
    }

    @Override
    public boolean apply(String[] args) {
        return isNotBlank(args[0])
            && isNotBlank(args[1])
            && args[0].endsWith(args[1]);
    }

}
