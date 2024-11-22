package com.legadi.cli.jurl.modifiers;

import java.util.function.Function;

public class PrefixValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "prefix";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "input" };
    }

    @Override
    public String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception {
        String input = propertyResolver.apply(args[0]);
        return input + value;
    }
}
