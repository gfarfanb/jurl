package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

import static com.legadi.jurl.model.AssertionType.NOT_EQUALS_TO;

import java.util.Objects;

public class NotEqualsToAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return NOT_EQUALS_TO.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "left", "right" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return !Objects.equals(args[0], args[1]);
    }

}
