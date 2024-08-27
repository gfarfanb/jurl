package com.legadi.cli.jurl.exception;

import java.util.Arrays;

public class ModifierException extends CommandException {

    public ModifierException(String name, String[] args, String[] values, String input, String message) {
        super("modifier=" + name + " args=" + Arrays.toString(args) + " values=" + Arrays.toString(values)
            + " input=" + input + (message != null ? " - " + message : ""));
    }
}
