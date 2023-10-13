package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.GeneratorType.INTEGER;

import com.legadi.jurl.common.Settings;

public class IntegerGenerator extends NumberGenerator {

    @Override
    public String tag() {
        return INTEGER.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            return numberPart(Integer.parseInt(arg));
        } else {
            return numberPart(DEFAULT_LENGTH);
        }
    }
}
