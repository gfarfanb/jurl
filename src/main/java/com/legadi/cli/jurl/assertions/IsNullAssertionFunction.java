package com.legadi.cli.jurl.assertions;

import java.util.Objects;

import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "IS_NULL")
public class IsNullAssertionFunction implements AssertionFunction {

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) {
        return Objects.isNull(args[0]);
    }

}
