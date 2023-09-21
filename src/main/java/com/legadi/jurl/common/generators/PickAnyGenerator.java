package com.legadi.jurl.common.generators;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class PickAnyGenerator implements Generator {

    private static final String PICK_ANY_PREFIX = "PICK_ANY:";

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(PICK_ANY_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String[] values = extractArg(PICK_ANY_PREFIX, param).split(",");
        return values[random.nextInt(values.length)];
    }
}
