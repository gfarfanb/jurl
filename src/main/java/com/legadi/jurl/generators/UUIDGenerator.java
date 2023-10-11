package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.UUID;

import com.legadi.jurl.common.Settings;

public class UUIDGenerator implements Generator {

    @Override
    public String tag() {
        return UUID.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        return java.util.UUID.randomUUID().toString();
    }
}
