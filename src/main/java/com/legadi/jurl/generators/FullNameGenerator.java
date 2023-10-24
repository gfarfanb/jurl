package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import com.legadi.jurl.common.Settings;

public class FullNameGenerator extends NamePartGenerator {

    @Override
    public String name() {
        return "FULL_NAME";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param) && WOMAN_GENDER.equalsIgnoreCase(param)) {
            return createFullName("woman-names.txt");
        } else {
            return createFullName("man-names.txt");
        }
    }

    private String createFullName(String namesFile) {
        String name = createNamePart(namesFile);
        String lastName = createNamePart("last-names.txt");
        return name + " " + lastName;
    }
}
