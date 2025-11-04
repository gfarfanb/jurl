package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.ConsoleInput;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "PASSWORD")
public class PasswordInputGenerator implements Generator {

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
