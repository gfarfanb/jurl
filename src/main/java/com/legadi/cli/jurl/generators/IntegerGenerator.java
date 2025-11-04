package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "INTEGER")
public class IntegerGenerator extends NumberGenerator {

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            return numberPart(Integer.parseInt(param));
        } else {
            return numberPart(DEFAULT_LENGTH);
        }
    }
}
