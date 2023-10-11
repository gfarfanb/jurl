package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.BOOLEAN;

import com.legadi.jurl.common.Settings;

public class BooleanGenerator implements Generator {

    @Override
    public String tag() {
        return BOOLEAN.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        return Boolean.toString(random().nextBoolean());
    }
}
