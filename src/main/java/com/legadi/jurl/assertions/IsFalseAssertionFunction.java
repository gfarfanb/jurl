package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.IS_FALSE;

import com.legadi.jurl.exception.AssertionException;

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
