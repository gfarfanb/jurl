package com.legadi.jurl.common.generators;

import java.util.UUID;

import com.legadi.jurl.common.Settings;

public class UUIDGenerator implements Generator {

    private static final String UUID_PREFIX = "UUID:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(UUID_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        return UUID.randomUUID().toString();
    }
}
