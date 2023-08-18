package com.legadi.jurl.common;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Settings {

    private static final Logger LOGGER = LoggerFactory.getLogger(Settings.class);
    private static final Properties APP_PROPERTIES = new Properties();

    static {
        String rootPath = Thread.currentThread().getContextClassLoader().getResource("").getPath();

        try {
            APP_PROPERTIES.load(new FileInputStream(rootPath + "application.properties"));
        } catch(IOException ex) {
            LOGGER.error("Error on loading properties", ex);
            throw new IllegalStateException("Error on loading properties");
        }
    }

    public String get(String propertyName) {
        return getValue(propertyName);
    }

    public String get(String propertyName, String defaultValue) {
        return getValueNoValidation(propertyName);
    }

    public <T> T get(String propertyName, Function<String, T> mapper) {
        return mapper.apply(getValue(propertyName));
    }

    public <T> T get(String propertyName, T defaultValue, Function<String, T> mapper) {
        String value = getValueNoValidation(propertyName);
        if(value == null) {
            return defaultValue;
        } else {
            return mapper.apply(value);
        }
    }

    private String getValue(String propertyName) {
        String value = APP_PROPERTIES.getProperty(propertyName);
        if(value == null) {
            throw new IllegalArgumentException("Property not found: " + propertyName);
        }
        return value;
    }

    private String getValueNoValidation(String propertyName) {
        return APP_PROPERTIES.getProperty(propertyName);
    }
}
