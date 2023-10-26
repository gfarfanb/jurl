package com.legadi.jurl.assertions;

import java.util.Objects;

public class IsNullAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_NULL";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return Objects.isNull(args[0]);
    }

}
