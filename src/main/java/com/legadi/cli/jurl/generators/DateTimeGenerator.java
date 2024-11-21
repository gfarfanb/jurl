package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.DateTimeFormatterUtil.getFormatter;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.legadi.cli.jurl.common.Settings;

public class DateTimeGenerator implements Generator {

    @Override
    public String name() {
        return "DATE-TIME";
    }

    @Override
    public String getValue(Settings settings, String param) {
        DateTimeFormatter formatter = getFormatter(param);
        return formatter.format(LocalDateTime.now());
    }
}
