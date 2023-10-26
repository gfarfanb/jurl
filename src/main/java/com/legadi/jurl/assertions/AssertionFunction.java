package com.legadi.jurl.assertions;

import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.model.AssertionEntry;

public interface AssertionFunction {

    String name();

    default String alias() {
        return "";
    }

    String[] getArgs();

    boolean apply(String[] args);

    default boolean accepts(String name) {
        return name().equalsIgnoreCase(name) || alias().equalsIgnoreCase(name);
    }

    default void evaluate(AssertionEntry entry, String message, String[] args) throws AssertionException {
        if(args == null) {
            throw new AssertionException(entry, getArgs(), args, "No arguments for assertion");
        }
        if(args.length < getArgs().length) {
            throw new AssertionException(entry, getArgs(), args, "Invalid number of arguments for assertion");
        }
        try {
            if(!apply(args)) {
                throw new AssertionException(entry, getArgs(), args, message);
            }
        } catch(NullPointerException ex) {
            throw new AssertionException(entry, getArgs(), args, "Null arguments are not allowed for assertion");
        }
    }
}
