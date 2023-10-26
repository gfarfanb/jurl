package com.legadi.jurl.assertions;

public class IsFalseAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_FALSE";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return !Boolean.valueOf(args[0]).booleanValue();
    }

}
