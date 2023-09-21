package com.legadi.jurl.common.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextNumber;

import com.legadi.jurl.common.Settings;

public class IntegerLengthGenerator implements Generator {

    private static final String INTEGER_LENGTH_PREFIX = "INTEGER_LENGTH:";
    private static final int DEFAULT_LENGTH = 10;

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(INTEGER_LENGTH_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(INTEGER_LENGTH_PREFIX, param);
        if(isNotBlank(arg)) {
            return nextNumber(Integer.parseInt(arg));
        } else {
            return nextNumber(DEFAULT_LENGTH);
        }
    }
}
