package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.model.AssertionType.IS_NOT_NULL;

import java.util.Objects;

public class IsNotNullAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_NOT_NULL.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return Objects.nonNull(args[0]);
    }

}
