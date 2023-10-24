package com.legadi.jurl.generators;

import java.util.Optional;

import com.legadi.jurl.common.Settings;

public class UserInputGenerator implements Generator {

    @Override
    public String name() {
        return "INPUT";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(settings.containsOverride(param)) {
            return settings.get(param);
        } else {
            String value = Optional.ofNullable(System.console())
                .map(console -> console.readLine(param))
                .orElse(param);
            settings.putOverride(param, value);
            return value;
        }
    }
}
