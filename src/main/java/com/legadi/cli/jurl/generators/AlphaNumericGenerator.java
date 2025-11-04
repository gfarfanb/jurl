package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.nextString;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "ALPHA_NUMERIC")
public class AlphaNumericGenerator implements Generator {

    public static final int RANGE_LENGTH = 10;

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            return nextString(Integer.parseInt(param));
        } else {
            return nextString(random().nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        }
    }
}
