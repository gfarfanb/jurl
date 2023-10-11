package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.INPUT;

import java.util.Optional;

import com.legadi.jurl.common.Settings;

public class UserInputGenerator implements Generator {

    @Override
    public String tag() {
        return INPUT.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(settings.containsOverride(param)) {
            return settings.get(param);
        } else {
            String message = extractArg(param);
            String value = Optional.ofNullable(System.console())
                .map(console -> console.readLine(message))
                .orElse(message);
            settings.putOverride(param, value);
            return value;
        }
    }
}
