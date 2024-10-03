package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.ConsoleInput;
import com.legadi.cli.jurl.common.Settings;

public class PasswordInputGenerator implements Generator {

    @Override
    public String name() {
        return "PASSWORD";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(settings.containsUserInput(param)) {
            return settings.get(param);
        } else {
            ConsoleInput consoleInput = new ConsoleInput(settings);
            String value = consoleInput.readPassword(param, null);
            settings.putUserInput(param, value);
            return value;
        }
    }
}
