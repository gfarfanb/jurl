package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextIndex;

import com.legadi.jurl.common.Settings;

public class PickAnyGenerator implements Generator {

    private static final String PICK_ANY_PREFIX = "PICK_ANY:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(PICK_ANY_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(PICK_ANY_PREFIX, param);
        if(isNotBlank(arg)) {
            String[] values = arg.split(",");
            return values[nextIndex(values.length)];
        } else {
            return "";
        }
    }
}
