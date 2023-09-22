package com.legadi.jurl.common.generators;

import java.io.Console;

import com.legadi.jurl.common.Settings;

public class PasswordInputGenerator implements Generator {

    private static final String PASSWORD_PREFIX = "PASSWORD:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(PASSWORD_PREFIX) && !settings.containsOverride(param);
    }

    @Override
    public String getValue(Settings settings, String param) {
        Console console = System.console();
        String value = String.valueOf(console.readPassword(extractArg(PASSWORD_PREFIX, param)));
        settings.putOverride(param, value);
        return value;
    }
}
