package com.legadi.jurl.exception;

import java.util.Arrays;

public class ModifierException extends CommandException {

    public ModifierException(String name, String[] args, String[] values, String message) {
        super("modifier=" + name + " args=" + Arrays.toString(args) + " values=" + Arrays.toString(values) + (message != null ? " - " + message : ""));
    }
}
