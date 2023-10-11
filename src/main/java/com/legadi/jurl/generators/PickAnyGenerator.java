package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextIndex;
import static com.legadi.jurl.model.GeneratorType.PICK_ANY;

import com.legadi.jurl.common.Settings;

public class PickAnyGenerator implements Generator {

    @Override
    public String tag() {
        return PICK_ANY.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            String[] values = arg.split(",");
            return values[nextIndex(values.length)].trim();
        } else {
            return "";
        }
    }
}
