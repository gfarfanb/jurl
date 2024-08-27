package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.nextIndex;
import static com.legadi.cli.jurl.common.CommonUtils.trim;

import com.legadi.cli.jurl.common.Settings;

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
