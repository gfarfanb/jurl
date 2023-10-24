package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import com.legadi.jurl.common.Settings;

public class IntegerGenerator extends NumberGenerator {

    @Override
    public String name() {
        return "INTEGER";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            return numberPart(Integer.parseInt(param));
        } else {
            return numberPart(DEFAULT_LENGTH);
        }
    }
}
