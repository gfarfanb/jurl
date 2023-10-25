package com.legadi.jurl.modifiers;

import java.util.function.Function;

import com.legadi.jurl.exception.InvalidModifierOperationException;

public class StringValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "string";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "(prefix|suffix)", "input" };
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        String input = getter.apply(args[1]);

        switch(args[0].toLowerCase()) {
            case "prefix":
                return input + value;
            case "suffix":
                return value + input;
            default:
                throw new InvalidModifierOperationException(args[0]);
        }
    }
}
