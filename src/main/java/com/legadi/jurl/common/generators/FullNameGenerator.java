package com.legadi.jurl.common.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import com.legadi.jurl.common.Settings;

public class FullNameGenerator extends NamePartGenerator {

    private static final String FULL_NAME_PREFIX = "FULL_NAME:";

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(FULL_NAME_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(FULL_NAME_PREFIX, param);
        if(isNotBlank(arg) && MAN_GENDER.equalsIgnoreCase(arg)) {
            return createFullName("man-names.txt");
        } else {
            return createFullName("woman-names.txt");
        }
    }

    private String createFullName(String namesFile) {
        String name = createNamePart(namesFile);
        String lastName = createNamePart("last-names.txt");
        return name + " " + lastName;
    }
}
