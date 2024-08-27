package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;

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
