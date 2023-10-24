package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextString;

import com.legadi.jurl.common.Settings;

public class AlphaNumericGenerator implements Generator {

    public static final int RANGE_LENGTH = 10;

    @Override
    public String name() {
        return "ALPHA_NUMERIC";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            return nextString(Integer.parseInt(param));
        } else {
            return nextString(random().nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        }
    }
}
