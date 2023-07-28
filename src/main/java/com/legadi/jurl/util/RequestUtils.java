package com.legadi.jurl.util;

import com.legadi.jurl.model.RequestEntry;

public class RequestUtils {

    public static boolean isHTTP(RequestEntry request) {
        String protocol = request.getUrl().getProtocol();
        return "http".equalsIgnoreCase(protocol) || "https".equalsIgnoreCase(protocol);
    }

    public static <T> T getOrDefault(T value, T defaultValue) {
        if(value != null) {
            return value;
        } else {
            return defaultValue;
        }
    }
}
