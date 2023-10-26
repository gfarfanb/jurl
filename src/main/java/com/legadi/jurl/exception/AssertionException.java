package com.legadi.jurl.exception;

import java.util.Arrays;

import com.legadi.jurl.model.AssertionEntry;

public class AssertionException extends RuntimeException {

    public AssertionException(AssertionEntry entry, String[] args, String[] values, String message) {
        super(entry.getType().name().toLowerCase()
            + "=" + entry.getName()
            + (entry.getAssertionClass() != null ? "(" + entry.getAssertionClass() + ")" : "")
            + " args=" + Arrays.toString(args)
            + " values=" + Arrays.toString(values) + (message != null ? " - " + message : ""));
    }
}
