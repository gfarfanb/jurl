package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.legadi.jurl.common.Settings;

public class DateTimeGenerator implements Generator {

    private static final String DATE_TIME_PREFIX = "DATE_TIME:";
    private static final DateTimeFormatter DEFAULT_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(DATE_TIME_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(DATE_TIME_PREFIX, param);
        if(isNotBlank(arg)) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(arg);
            return formatter.format(LocalDateTime.now());
        } else {
            return DEFAULT_FORMATTER.format(LocalDateTime.now());
        }
    }
}
