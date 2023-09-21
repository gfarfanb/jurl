package com.legadi.jurl.common.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextString;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class AlphaNumericGenerator implements Generator {

    private static final String ALPHA_NUMERIC_PREFIX = "ALPHA_NUMERIC:";
    private static final int RANGE_LENGTH = 10;

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(ALPHA_NUMERIC_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(ALPHA_NUMERIC_PREFIX, param);
        if(isNotBlank(arg)) {
            return nextString(Integer.parseInt(arg));
        } else {
            return nextString(random.nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        }
    }
}
