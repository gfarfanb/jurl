package com.legadi.jurl.generators;

import static com.legadi.jurl.model.GeneratorType.LAST_NAME;

import com.legadi.jurl.common.Settings;

public class LastNameGenerator extends NamePartGenerator {

    @Override
    public String tag() {
        return LAST_NAME.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        return createNamePart("last-names.txt");
    }
}
