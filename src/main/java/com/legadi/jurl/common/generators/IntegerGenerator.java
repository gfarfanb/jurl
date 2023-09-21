package com.legadi.jurl.common.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class IntegerGenerator implements Generator {

    private static final String INTEGER_PREFIX = "INTEGER:";

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(INTEGER_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(INTEGER_PREFIX, param);
        if(isNotBlank(arg)) {
            return Integer.toString(random.nextInt(Integer.parseInt(arg)));
        } else {
            return Integer.toString(random.nextInt());
        }
    }
}
