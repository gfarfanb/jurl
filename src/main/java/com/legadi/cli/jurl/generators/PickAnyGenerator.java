package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.nextIndex;
import static com.legadi.cli.jurl.common.CommonUtils.trim;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "PICK_ANY")
public class PickAnyGenerator implements Generator {

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            String[] values = param.split(",");
            return trim(values[nextIndex(values.length)]);
        } else {
            return "";
        }
    }
}
