package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.GeneratorType.FULL_NAME;

import com.legadi.jurl.common.Settings;

public class FullNameGenerator extends NamePartGenerator {

    @Override
    public String tag() {
        return FULL_NAME.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg) && WOMAN_GENDER.equalsIgnoreCase(arg)) {
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
