package com.legadi.jurl.exception;

import java.util.Arrays;

public class AssertionException extends RuntimeException {

    public AssertionException(String name, String[] args, String message) {
        super("assertion=" + name + " args=" + Arrays.toString(args) + " - " + message);
    }
}
