package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextNumber;
import static com.legadi.jurl.model.GeneratorType.DECIMAL;

import com.legadi.jurl.common.Settings;

public class DecimalGenerator implements Generator {

    public static final int DEFAULT_LENGTH = 5;
    public static final int DEFAULT_DECIMAL_LENGTH = 2;

    @Override
    public String tag() {
        return DECIMAL.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            String[] parts = arg.split(",");
            if(parts.length >= 2) {
                return nextNumber(Integer.parseInt(parts[0])) + "." + nextNumber(Integer.parseInt(parts[1]));
            } else {
                return nextNumber(Integer.parseInt(parts[0])) + "." + nextNumber(DEFAULT_DECIMAL_LENGTH);
            }
        } else {
            return nextNumber(DEFAULT_LENGTH) + "." + nextNumber(DEFAULT_DECIMAL_LENGTH);
        }
    }
}
