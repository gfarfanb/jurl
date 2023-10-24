package com.legadi.jurl.modifiers;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.InvalidModifierOperationException;

public class StringValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "String";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "(prefix|suffix)", "input" };
    }

    @Override
    public String apply(Settings settings, String[] args, String value) throws Exception {
        String input = settings.getOrDefault(args[1], args[1]);

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
