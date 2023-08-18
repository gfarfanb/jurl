package com.legadi.jurl.common;

public class StringUtils {

    public static boolean isNotBlank(String value) {
        return value != null && !value.trim().isEmpty();
    }

    public static boolean isBlank(String value) {
        return !isNotBlank(value);
    }

    public static String strip(String value, String stripChars) {
        value = stripStart(value, stripChars);
        return stripEnd(value, stripChars);
    }

    public static String stripStart(String value, String stripChars) {
        int length = (value == null ? 0 : value.length());
        if(length == 0) {
            return value;
        }
        int index = 0;
        if(stripChars == null) {
            while(index != length && Character.isWhitespace(value.charAt(index))) {
                index++;
            }
        } else if(stripChars.isEmpty()) {
            return value;
        } else {
            while(index != length && stripChars.indexOf(value.charAt(index)) != -1) {
                index++;
            }
        }
        return value.substring(index);
    }

    public static String stripEnd(String value, String stripChars) {
        int length = (value == null ? 0 : value.length());
        if(length == 0) {
            return value;
        }
        int index = length - 1;
        if(stripChars == null) {
            while(index != 0 && Character.isWhitespace(value.charAt(index))) {
                index--;
            }
        } else if(stripChars.isEmpty()) {
            return value;
        } else {
            while(index != 0 && stripChars.indexOf(value.charAt(index)) != -1) {
                index--;
            }
        }
        return value.substring(0, index);
    }
}
