package com.legadi.jurl.common.generators;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class DecimalGenerator implements Generator {

    private static final String DECIMAL_PREFIX = "DECIMAL:";

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(DECIMAL_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        return Float.toString(random.nextFloat());
    }
    
}
