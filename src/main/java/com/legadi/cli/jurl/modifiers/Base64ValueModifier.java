package com.legadi.cli.jurl.modifiers;

import java.util.Base64;
import java.util.function.Function;

import com.legadi.cli.jurl.exception.CommandException;

public class Base64ValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "base64";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "operation" };
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        String input = propertyResolver.apply(value);
        String operation = args[0].toLowerCase();

        switch(operation) {
            case "encode":
                return new String(Base64.getEncoder().encode(input.getBytes()));
            case "decode":
                return new String(Base64.getDecoder().decode(input.getBytes()));
            default:
                throw new CommandException("Invalid Base 64 operation: " + operation);
        }
    }
}
