package com.legadi.jurl.common;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.regex.Pattern;

import com.google.gson.Gson;

public class CommonUtils {

    private static final Gson GSON = new Gson();
    private static final Pattern NUMBER_PATTERN = Pattern.compile("-?\\d+(\\.\\d+)?");
    private static final String ALPHA_NUMERIC_STRING = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvxyz0123456789";
    private static final String NUMERIC_STRING = "0123456789";

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
            while(index != length
                    && (stripChars.indexOf(value.charAt(index)) != -1
                        || Character.isWhitespace(value.charAt(index)))) {
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
        if(stripChars == null) {
            while(length != 0 && Character.isWhitespace(value.charAt(length - 1))) {
                length--;
            }
        } else if(stripChars.isEmpty()) {
            return value;
        } else {
            while(length != 0
                    && (stripChars.indexOf(value.charAt(length - 1)) != -1
                        || Character.isWhitespace(value.charAt(length - 1)))) {
                length--;
            }
        }
        return value.substring(0, length);
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

    public static String nextString(int length) {
        return nextString(length, ALPHA_NUMERIC_STRING.length(), (i, index) ->
            ALPHA_NUMERIC_STRING.charAt(index));
    }

    public static String nextNumber(int length) {
        return nextString(length, NUMERIC_STRING.length(), (i, index) ->
            NUMERIC_STRING.charAt(i == 0 && index == 0 ? 1 : index));
    }

    public static <T> String nextString(int length, int sourceLength,
            BiFunction<Integer, Integer, T> indexMapper) {
        StringBuilder elements = new StringBuilder();
        int index;

        for (int i = 0; i < length; i++) {
            index = nextIndex(sourceLength);
            elements.append(indexMapper.apply(i, index));
        }

        return elements.toString();
    }

    public static int nextIndex(int length) {
        return(int) (length * Math.random());
    }

    public static Path createDirectories(Path path) {
        try {
            Files.createDirectories(path);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create directory: " + path, ex);
        }
        return path;
    }
}
