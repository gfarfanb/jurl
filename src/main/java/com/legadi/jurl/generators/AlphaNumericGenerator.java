package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.model.GeneratorType.ALPHA_NUMERIC;

import com.legadi.jurl.common.Settings;

public class AlphaNumericGenerator implements Generator {

    public static final int RANGE_LENGTH = 10;

    @Override
    public String tag() {
        return ALPHA_NUMERIC.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            return nextString(Integer.parseInt(arg));
        } else {
            return nextString(random().nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        }
    }
}
