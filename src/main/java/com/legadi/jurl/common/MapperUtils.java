package com.legadi.jurl.common;

public class MapperUtils {

    public static <T> T getOrDefault(T value, T defaultValue) {
        if(value != null) {
            return value;
        } else {
            return defaultValue;
        }
    }
}
