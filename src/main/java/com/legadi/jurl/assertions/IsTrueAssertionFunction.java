package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.model.AssertionType.IS_TRUE;

public class IsTrueAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_TRUE.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return Boolean.valueOf(args[0]).booleanValue();
    }

}
