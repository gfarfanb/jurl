package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isBlank;

import java.time.format.DateTimeFormatter;

public class DateTimeFormatterUtil {

    public static final DateTimeFormatter DEFAULT_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    private DateTimeFormatterUtil() {}

    public static DateTimeFormatter getFormatter(String param) {
        if(isBlank(param)) {
            return DEFAULT_FORMATTER;
        }

        switch(param) {
            case "BASIC_ISO_DATE":
                return DateTimeFormatter.BASIC_ISO_DATE;
            case "ISO_LOCAL_DATE":
                return DateTimeFormatter.ISO_LOCAL_DATE;
            case "ISO_OFFSET_DATE":
                return DateTimeFormatter.ISO_OFFSET_DATE;
            case "ISO_DATE":
                return DateTimeFormatter.ISO_DATE;
            case "ISO_LOCAL_TIME":
                return DateTimeFormatter.ISO_LOCAL_TIME;
            case "ISO_OFFSET_TIME":
                return DateTimeFormatter.ISO_OFFSET_TIME;
            case "ISO_TIME":
                return DateTimeFormatter.ISO_TIME;
            case "ISO_LOCAL_DATE_TIME":
                return DateTimeFormatter.ISO_LOCAL_DATE_TIME;
            case "ISO_OFFSET_DATE_TIME":
                return DateTimeFormatter.ISO_OFFSET_DATE_TIME;
            case "ISO_ZONED_DATE_TIME":
                return DateTimeFormatter.ISO_ZONED_DATE_TIME;
            case "ISO_DATE_TIME":
                return DateTimeFormatter.ISO_DATE_TIME;
            case "ISO_ORDINAL_DATE":
                return DateTimeFormatter.ISO_ORDINAL_DATE;
            case "ISO_WEEK_DATE":
                return DateTimeFormatter.ISO_WEEK_DATE;
            case "ISO_INSTANT":
                return DateTimeFormatter.ISO_INSTANT;
            case "RFC_1123_DATE_TIME":
                return DateTimeFormatter.RFC_1123_DATE_TIME;
            default:
                return DateTimeFormatter.ofPattern(param);
        }
    }
}
