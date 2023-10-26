package com.legadi.jurl.assertions;

public class IsTrueAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_TRUE";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return Boolean.valueOf(args[0]).booleanValue();
    }

}
