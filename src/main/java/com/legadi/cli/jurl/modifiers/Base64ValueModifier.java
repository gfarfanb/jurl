package com.legadi.cli.jurl.modifiers;

import java.util.Base64;
import java.util.function.Function;

public class Base64ValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "base64";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        String input = propertyResolver.apply(value);
        byte[] encoded = Base64.getEncoder().encode(input.getBytes());
        return new String(encoded);
    }
}
