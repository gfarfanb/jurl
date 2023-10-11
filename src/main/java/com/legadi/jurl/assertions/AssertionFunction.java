package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;

public interface AssertionFunction {

    String name();

    String[] getArgs();

    boolean apply(String[] args) throws AssertionException;

    default boolean accepts(String name) {
        return name().equalsIgnoreCase(name);
    }

    default void evaluate(String message, String[] args) throws AssertionException {
        if(args == null) {
            throw new AssertionException(name(), getArgs(), args, "No arguments for assertion");
        }
        if(args.length < getArgs().length) {
            throw new AssertionException(name(), getArgs(), args, "Invalid number of arguments for assertion");
        }
        try {
            if(!apply(args)) {
                throw new AssertionException(name(), getArgs(), args, message);
            }
        } catch(NullPointerException ex) {
            throw new AssertionException(name(), getArgs(), args, "Null arguments are not allowed for assertion");
        }
    }
}
