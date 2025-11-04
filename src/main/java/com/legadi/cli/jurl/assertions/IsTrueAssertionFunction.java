package com.legadi.cli.jurl.assertions;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_TRUE")
public class IsTrueAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return Boolean.valueOf(args[0]).booleanValue();
    }

}
