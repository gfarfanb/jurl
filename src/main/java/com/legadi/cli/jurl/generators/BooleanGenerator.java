package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;

public class BooleanGenerator implements Generator {

    @Override
    public String name() {
        return "BOOLEAN";
    }

    @Override
    public String getValue(Settings settings, String param) {
        return Boolean.toString(random().nextBoolean());
    }
}
