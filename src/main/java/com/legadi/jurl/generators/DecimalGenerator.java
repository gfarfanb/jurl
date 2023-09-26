package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextNumber;

import java.util.Random;

import com.legadi.jurl.common.Settings;

public class DecimalGenerator implements Generator {

    private static final String DECIMAL_PREFIX = "DECIMAL:";
    private static final int DEFAULT_LENGTH = 5;
    private static final int DEFAULT_DECIMAL_LENGTH = 2;

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(DECIMAL_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(DECIMAL_PREFIX, param);
        if(isNotBlank(arg)) {
            String[] parts = arg.split(",");
            if(parts.length >= 2) {
                return nextNumber(Integer.parseInt(parts[0])) + "." + nextNumber(Integer.parseInt(parts[1]));
            } else {
                return nextNumber(Integer.parseInt(parts[0])) + "." + nextNumber(DEFAULT_DECIMAL_LENGTH);
            }
        } else if(random.nextBoolean()) {
            return nextNumber(DEFAULT_LENGTH) + "." + nextNumber(DEFAULT_DECIMAL_LENGTH);
        } else {
            return Float.toString(random.nextFloat());
        }
    }
}
