package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "UUID")
public class UUIDGenerator implements Generator {

    @Override
    public String getValue(Settings settings, String param) {
        return java.util.UUID.randomUUID().toString();
    }
}
