package com.legadi.jurl.assertions;

import java.util.Arrays;

import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.exception.CommandException;

public interface AssertionFunction {

    String name();

    String[] getArgs();

    boolean apply(String[] args) throws AssertionException;

    default void evaluate(String message, String[] args) throws AssertionException {
        if(args == null) {
            throw new CommandException("No arguments for assertion: " + name()
                + "(" + String.join(",", getArgs()) + ")");
        }
        if(args.length < getArgs().length) {
            throw new CommandException("Invalid number of arguments for assertion: " + name()
                + "(" + String.join(",", getArgs()) + ") - " + Arrays.toString(args));
        }
        if(!apply(args)) {
            throw new AssertionException(name(), args, message);
        }
    }
}
