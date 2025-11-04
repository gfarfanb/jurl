package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "EMPTY")
public class EmptyGenerator implements Generator {

    @Override
    public String getValue(Settings settings, String param) {
        return "";
    }
}
