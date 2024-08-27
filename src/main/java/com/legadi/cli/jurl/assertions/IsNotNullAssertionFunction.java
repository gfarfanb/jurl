package com.legadi.cli.jurl.assertions;

import java.util.Objects;

public class IsNotNullAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return "IS_NOT_NULL";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return Objects.nonNull(args[0]);
    }

}
