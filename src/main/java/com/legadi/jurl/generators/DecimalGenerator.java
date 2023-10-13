package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.GeneratorType.DECIMAL;

import com.legadi.jurl.common.Settings;

public class DecimalGenerator extends NumberGenerator {

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
                return numberPart(Integer.parseInt(parts[0])) + "." + numberPart(Integer.parseInt(parts[1]));
            } else {
                return numberPart(Integer.parseInt(parts[0])) + "." + numberPart(DEFAULT_DECIMAL_LENGTH);
            }
        } else {
            return numberPart(DEFAULT_LENGTH) + "." + numberPart(DEFAULT_DECIMAL_LENGTH);
        }
    }

}
