package com.legadi.jurl.generators;

import com.legadi.jurl.common.Settings;

public class LastNameGenerator extends NamePartGenerator {

    @Override
    public String name() {
        return "LAST_NAME";
    }

    @Override
    public String getValue(Settings settings, String param) {
        return createNamePart("last-names.txt");
    }
}
