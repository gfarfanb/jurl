package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.model.AssertionType.IS_FALSE;

public class IsFalseAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return IS_FALSE.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return !Boolean.valueOf(args[0]).booleanValue();
    }

}
