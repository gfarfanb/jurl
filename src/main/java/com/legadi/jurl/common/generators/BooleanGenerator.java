package com.legadi.jurl.common.generators;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class BooleanGenerator implements Generator {

    private static final String BOOLEAN_PREFIX = "BOOLEAN:";

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(BOOLEAN_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        return Boolean.toString(random.nextBoolean());
    }
}
