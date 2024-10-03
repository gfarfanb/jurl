package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.ConsoleInput;
import com.legadi.cli.jurl.common.Settings;

public class UserInputGenerator implements Generator {

    @Override
    public String name() {
        return "INPUT";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(settings.containsUserInput(param)) {
            return settings.get(param);
        } else {
            ConsoleInput consoleInput = new ConsoleInput(settings);
            String value = consoleInput.readInput(param, null);
            settings.putUserInput(param, value);
            return value;
        }
    }
}
