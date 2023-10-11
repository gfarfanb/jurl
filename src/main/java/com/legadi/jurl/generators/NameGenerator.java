package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.GeneratorType.NAME;

import com.legadi.jurl.common.Settings;

public class NameGenerator extends NamePartGenerator {

    @Override
    public String tag() {
        return NAME.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg) && WOMAN_GENDER.equalsIgnoreCase(arg)) {
            return createNamePart("woman-names.txt");
        } else {
            return createNamePart("man-names.txt");
        }
    }
}
