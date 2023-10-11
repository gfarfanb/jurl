package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.PASSWORD;

import java.util.Optional;

import com.legadi.jurl.common.Settings;

public class PasswordInputGenerator implements Generator {

    @Override
    public String tag() {
        return PASSWORD.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(settings.containsOverride(param)) {
            return settings.get(param);
        } else {
            String message = extractArg(param);
            String value = Optional.ofNullable(System.console())
                .map(console -> console.readPassword(message))
                .map(String::valueOf)
                .orElse(message);
            settings.putOverride(param, value);
            return value;
        }
    }
}
