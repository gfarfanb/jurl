package com.legadi.jurl.generators;

import static com.legadi.jurl.common.DateTimeFormatterUtil.getFormatter;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.legadi.jurl.common.Settings;

public class DateTimeGenerator implements Generator {

    @Override
    public String name() {
        return "DATE_TIME";
    }

    @Override
    public String getValue(Settings settings, String param) {
        DateTimeFormatter formatter = getFormatter(param);
        return formatter.format(LocalDateTime.now());
    }
}
