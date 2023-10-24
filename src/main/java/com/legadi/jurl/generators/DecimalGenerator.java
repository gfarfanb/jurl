package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import com.legadi.jurl.common.Settings;

public class DecimalGenerator extends NumberGenerator {

    @Override
    public String name() {
        return "DECIMAL";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            String[] parts = param.split(",");
            if(parts.length >= 2) {
                return numberPart(Integer.parseInt(parts[0])) + "." + numberPart(Integer.parseInt(parts[1]));
            } else {
                return numberPart(Integer.parseInt(parts[0])) + "." + numberPart(DEFAULT_DECIMAL_LENGTH);
            }
        } else {
            return numberPart(DEFAULT_LENGTH) + "." + numberPart(DEFAULT_DECIMAL_LENGTH);
        }
    }

}
