package com.legadi.jurl.asserts;

import com.legadi.jurl.exception.AssertException;

public interface AssertFunction {

    boolean accepts(String name);

    void apply(String message, String[] args) throws AssertException;
}
