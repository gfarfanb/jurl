package com.legadi.cli.jurl.assertions;

import java.util.Objects;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_NOT_NULL")
public class IsNotNullAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return Objects.nonNull(args[0]);
    }

}
