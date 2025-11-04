package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "LAST_NAME")
public class LastNameGenerator extends NamePartGenerator {

    @Override
    public String getValue(Settings settings, String param) {
        return createNamePart("last-names.txt");
    }
}
