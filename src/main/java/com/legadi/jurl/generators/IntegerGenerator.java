package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextNumber;
import static com.legadi.jurl.model.GeneratorType.INTEGER;

import com.legadi.jurl.common.Settings;

public class IntegerGenerator implements Generator {

    public static final int DEFAULT_LENGTH = 5;

    @Override
    public String tag() {
        return INTEGER.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            return nextNumber(Integer.parseInt(arg));
        } else {
            return nextNumber(DEFAULT_LENGTH);
        }
    }
}
