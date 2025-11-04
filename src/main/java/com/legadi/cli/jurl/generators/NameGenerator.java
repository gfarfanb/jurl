package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "NAME")
public class NameGenerator extends NamePartGenerator {

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param) && WOMAN_GENDER.equalsIgnoreCase(param)) {
            return createNamePart("woman-names.txt");
        } else {
            return createNamePart("man-names.txt");
        }
    }
}
