package com.legadi.cli.jurl.assertions;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "DOES_NOT_CONTAINS")
public class DoesNotContainsAssertionFunction implements AssertionFunction {

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
