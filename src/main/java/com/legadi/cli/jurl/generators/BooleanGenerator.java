package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "BOOLEAN")
public class BooleanGenerator implements Generator {

    @Override
    public String getValue(Settings settings, String param) {
        return Boolean.toString(random().nextBoolean());
    }
}
