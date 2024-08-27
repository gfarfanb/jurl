package com.legadi.cli.jurl.modifiers;

import java.util.function.Function;

public class WindowsSeparatorValueModifier implements ValueModifier {

    @Override
    public String name() {
        return "win-separator";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String apply(Function<String, String> getter, String[] args, String value) throws Exception {
        return value.replaceAll("\\\\", "\\\\\\\\");
    }
}
