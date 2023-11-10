package com.legadi.jurl.modifiers;

import static com.legadi.jurl.common.CommonUtils.isBlank;

import java.util.function.Function;

public class DefaultValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "default";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "default-value" };
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        if(isBlank(value)) {
            return args[0];
        } else {
            return value;
        }
    }
}
