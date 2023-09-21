package com.legadi.jurl.common.generators;

import com.legadi.jurl.common.Settings;

public interface Generator {

    boolean accepts(Settings settings, String param);

    default String get(Settings settings, String param) {
        try {
            return getValue(settings, param);
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to generate value - " + ex.getMessage(), ex);
        }
    }

    String getValue(Settings settings, String param);

    default String extractArg(String prefix, String param) {
        return param.substring(prefix.length(), param.length());
    }
}
