package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertException;

public interface AssertionFunction {

    boolean accepts(String name);

    void apply(String message, String[] args) throws AssertException;
}
