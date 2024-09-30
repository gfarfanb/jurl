package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.PASSWORD_FORMAT;

import java.util.Optional;

import com.legadi.cli.jurl.common.CommonUtils;
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
            String value = Optional.ofNullable(System.console())
                .map(console -> console.readPassword(String.format(PASSWORD_FORMAT, param)))
                .map(String::valueOf)
                .filter(CommonUtils::isNotBlank)
                .orElse(param);
            settings.putUserInput(param, value);
            return value;
        }
    }
}
