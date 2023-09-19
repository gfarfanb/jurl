package com.legadi.jurl.assertions;

import static com.legadi.jurl.model.AssertionType.EQUALS_TO;

import java.util.Objects;

import com.legadi.jurl.exception.AssertionException;

public class EqualsToAssertionFunction implements AssertionFunction {

    @Override
    public String name() {
        return EQUALS_TO.name();
    }

    @Override
    public String[] getArgs() {
        return new String[] { "left", "right" };
    }

    @Override
    public boolean apply(String[] args) throws AssertionException {
        return Objects.equals(args[0], args[1]);
    }

}
