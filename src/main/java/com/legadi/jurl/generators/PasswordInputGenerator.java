package com.legadi.jurl.generators;

import java.util.Optional;

import com.legadi.jurl.common.Settings;

public class PasswordInputGenerator implements Generator {

    @Override
    public String name() {
        return "PASSWORD";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(settings.containsOverride(param)) {
            return settings.get(param);
        } else {
            String value = Optional.ofNullable(System.console())
                .map(console -> console.readPassword(param))
                .map(String::valueOf)
                .orElse(param);
            settings.putOverride(param, value);
            return value;
        }
    }
}
