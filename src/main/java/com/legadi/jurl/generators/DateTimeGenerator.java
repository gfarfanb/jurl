package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.legadi.jurl.common.Settings;

public class DateTimeGenerator implements Generator {

    public static final DateTimeFormatter DEFAULT_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    @Override
    public String name() {
        return "DATE_TIME";
    }

    @Override
    public String getValue(Settings settings, String param) {
        if(isNotBlank(param)) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(param);
            return formatter.format(LocalDateTime.now());
        } else {
            return DEFAULT_FORMATTER.format(LocalDateTime.now());
        }
    }
}
