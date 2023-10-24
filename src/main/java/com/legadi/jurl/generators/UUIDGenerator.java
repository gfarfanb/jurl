package com.legadi.jurl.generators;

import com.legadi.jurl.common.Settings;

public class UUIDGenerator implements Generator {

    @Override
    public String name() {
        return "UUID";
    }

    @Override
    public String getValue(Settings settings, String param) {
        return java.util.UUID.randomUUID().toString();
    }
}
