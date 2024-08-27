package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import com.legadi.cli.jurl.common.Settings;

public class NameGenerator extends NamePartGenerator {

    @Override
    public String name() {
        return "NAME";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param) && WOMAN_GENDER.equalsIgnoreCase(param)) {
            return createNamePart("woman-names.txt");
        } else {
            return createNamePart("man-names.txt");
        }
    }
}
