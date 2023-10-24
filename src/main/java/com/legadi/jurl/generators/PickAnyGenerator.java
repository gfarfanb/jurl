package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextIndex;
import static com.legadi.jurl.common.CommonUtils.trim;

import com.legadi.jurl.common.Settings;

public class PickAnyGenerator implements Generator {

    @Override
    public String name() {
        return "PICK_ANY";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            String[] values = param.split(",");
            return trim(values[nextIndex(values.length)]);
        } else {
            return "";
        }
    }
}
