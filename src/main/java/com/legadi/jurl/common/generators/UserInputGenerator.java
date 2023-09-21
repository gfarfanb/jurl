package com.legadi.jurl.common.generators;

import java.io.Console;

import com.legadi.jurl.common.Settings;

public class UserInputGenerator implements Generator {

    private static final String INPUT_PREFIX = "INPUT:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(INPUT_PREFIX) || !settings.containsOverride(param);
    }

    @Override
    public String getValue(Settings settings, String param) {
        Console console = System.console();
        String value = console.readLine(extractArg(INPUT_PREFIX, param));
        settings.putOverride(param, value);
        return value;
    }
}
