package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import com.legadi.jurl.common.Settings;

public class NameGenerator extends NamePartGenerator {

    private static final String NAME_PREFIX = "NAME:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(NAME_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(NAME_PREFIX, param);
        if(isNotBlank(arg) && MAN_GENDER.equalsIgnoreCase(arg)) {
            return createNamePart("man-names.txt");
        } else {
            return createNamePart("woman-names.txt");
        }
    }
}
