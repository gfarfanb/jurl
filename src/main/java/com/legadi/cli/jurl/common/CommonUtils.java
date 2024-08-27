package com.legadi.cli.jurl.common;

import java.io.File;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.regex.Pattern;

public class CommonUtils {

    private static final Pattern NUMBER_PATTERN = Pattern.compile("-?\\d+(\\.\\d+)?");

    public static final String ALPHA_NUMERIC_STRING = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvxyz0123456789";
    public static final String NUMERIC_STRING = "0123456789";

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

    public static String trim(String value) {
        return strip(value, null);
    }

    public static String fileSeparatorAsDelimiter() {
        if(File.separator.equals("\\")) {
            return "\\\\";
        } else {
            return File.separator;
        }
    }

    public static String strip(String value, String stripChars) {
        value = stripStart(value, stripChars);
        return stripEnd(value, stripChars);
    }

    public static String stripStart(String value, String stripChars) {
        if(value == null) {
            return value;
        }
        stripChars = stripChars != null && stripChars.isEmpty() ? null : stripChars;
        int index = 0;
        if(stripChars == null) {
            while(index != value.length() && Character.isWhitespace(value.charAt(index))) {
                index++;
            }
        } else {
            while(index != value.length()
                    && (stripChars.indexOf(value.charAt(index)) != -1
                        || Character.isWhitespace(value.charAt(index)))) {
                index++;
            }
        }
        return value.substring(index);
    }

    public static String stripEnd(String value, String stripChars) {
        if(value == null) {
            return value;
        }
        stripChars = stripChars != null && stripChars.isEmpty() ? null : stripChars;
        int length = value.length();
        if(stripChars == null) {
            while(length != 0 && Character.isWhitespace(value.charAt(length - 1))) {
                length--;
            }
        } else {
            while(length != 0
                    && (stripChars.indexOf(value.charAt(length - 1)) != -1
                        || Character.isWhitespace(value.charAt(length - 1)))) {
                length--;
            }
        }
        return value.substring(0, length);
    }

    public static boolean isNotNumeric(String value) {
        return !isNumeric(value);
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

    public static String avoidFirstZero(String number, String zeroReplacer) {
        if(!isNumeric(number)) {
            return number;
        }
        char firstChar = number.charAt(0);
        if(firstChar == '0') {
            if(number.length() == 1) {
                return zeroReplacer;
            } else {
                return zeroReplacer + number.substring(1);
            }
        } else {
            return number;
        }
    }

    public static String nextString(int length) {
        return nextString(length, ALPHA_NUMERIC_STRING.length(),
            (i, randomIndex) -> ALPHA_NUMERIC_STRING.charAt(randomIndex));
    }

    public static String nextNumber(int length) {
        return nextString(length, NUMERIC_STRING.length(),
            (i, randomIndex) -> NUMERIC_STRING.charAt(randomIndex));
    }

    public static <T> String nextString(int length, int sourceLength,
            BiFunction<Integer, Integer, T> indexMapper) {
        StringBuilder elements = new StringBuilder();
        int randomIndex;

        for (int i = 0; i < length; i++) {
            randomIndex = nextIndex(sourceLength);
            elements.append(indexMapper.apply(i, randomIndex));
        }

        return elements.toString();
    }

    public static int nextIndex(int length) {
        return (int) (length * Math.random());
    }

    public static Map<String, Field> getAllFields(Class<?> type) {
        Map<String, Field> fields = new HashMap<>();

        while (type != null && type != Object.class) {
            Arrays.stream(type.getDeclaredFields())
                .forEach(f -> fields.put(f.getName(), f));

            type = type.getSuperclass();
        }

        return fields;
    }
}
