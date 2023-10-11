package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.model.GeneratorType.DATE_TIME;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.legadi.jurl.common.Settings;

public class DateTimeGenerator implements Generator {

    public static final DateTimeFormatter DEFAULT_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    @Override
    public String tag() {
        return DATE_TIME.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(arg);
            return formatter.format(LocalDateTime.now());
        } else {
            return DEFAULT_FORMATTER.format(LocalDateTime.now());
        }
    }
}
