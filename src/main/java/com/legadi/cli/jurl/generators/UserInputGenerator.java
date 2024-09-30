package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.INPUT_FORMAT;

import java.util.Optional;

import com.legadi.cli.jurl.common.CommonUtils;
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
            String value = Optional.ofNullable(System.console())
                .map(console -> console.readLine(String.format(INPUT_FORMAT, param)))
                .filter(CommonUtils::isNotBlank)
                .orElse(param);
            settings.putUserInput(param, value);
            return value;
        }
    }
}
