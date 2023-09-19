package com.legadi.jurl.common;

import java.util.Collection;
import java.util.Map;
import java.util.regex.Pattern;

import com.google.gson.Gson;

public class CommonUtils {

    private static final Gson GSON = new Gson();
    private static Pattern NUMBER_PATTERN = Pattern.compile("-?\\d+(\\.\\d+)?");

    private CommonUtils() {}

    public static boolean isNotBlank(String value) {
        return value != null && !value.trim().isEmpty();
    }

    public static boolean isBlank(String value) {
        return !isNotBlank(value);
    }

    public static boolean isNotEmpty(Collection<?> collection) {
        return collection != null && !collection.isEmpty();
    }

    public static boolean isEmpty(Collection<?> collection) {
        return !isNotEmpty(collection);
    }

    public static boolean isNotEmpty(Map<?, ?> map) {
        return map != null && !map.isEmpty();
    }

    public static boolean isEmpty(Map<?, ?> map) {
        return !isNotEmpty(map);
    }

    public static boolean isNotEmpty(Object[] array) {
        return array != null && array.length > 0;
    }

    public static boolean isEmpty(Object[] array) {
        return !isNotEmpty(array);
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

    public static String toJsonString(Object value) {
        return GSON.toJson(value);
    }

    public static boolean isNumeric(String value) {
        if (value == null) {
            return false; 
        }
        return NUMBER_PATTERN.matcher(value).matches();
    }

    public static <T> T getOrDefault(T value, T defaultValue) {
        if(value == null) {
            return defaultValue;
        } else {
            return value;
        }
    }
}