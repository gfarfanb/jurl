package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.DateTimeFormatterUtil.getFormatter;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "DATE_TIME")
public class DateTimeGenerator implements Generator {

    @Override
    public String getValue(Settings settings, String param) {
        DateTimeFormatter formatter = getFormatter(param);
        return formatter.format(OffsetDateTime.now());
    }
}
