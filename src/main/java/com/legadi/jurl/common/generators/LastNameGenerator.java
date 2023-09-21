package com.legadi.jurl.common.generators;

import com.legadi.jurl.common.Settings;

public class LastNameGenerator extends NamePartGenerator {

    private static final String LAST_NAME_PREFIX = "LAST_NAME:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(LAST_NAME_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        return createNamePart("last-names.txt");
    }
}
