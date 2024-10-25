package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;

public class EmptyGenerator implements Generator {

    @Override
    public String name() {
        return "EMPTY";
    }

    @Override
    public String getValue(Settings settings, String param) {
        return "";
    }
}
