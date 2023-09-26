package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextNumber;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class IntegerGenerator implements Generator {

    private static final String INTEGER_PREFIX = "INTEGER:";
    private static final int DEFAULT_LENGTH = 5;

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(INTEGER_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(INTEGER_PREFIX, param);
        if(isNotBlank(arg)) {
            return nextNumber(Integer.parseInt(arg));
        } else if(random.nextBoolean()) {
            return nextNumber(DEFAULT_LENGTH);
        } else {
            return Integer.toString(random.nextInt());
        }
    }
}
