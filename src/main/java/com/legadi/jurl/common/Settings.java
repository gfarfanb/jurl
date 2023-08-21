package com.legadi.jurl.common;

import java.io.IOException;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

public class Settings {

    private static final Map<String, String> SETTINGS = new HashMap<>();

    static {
        Path settingsPath;

        try {
            settingsPath = Paths.get(
                Thread
                    .currentThread()
                    .getContextClassLoader()
                    .getResource("settings.json")
                    .toURI()
            );
        } catch(URISyntaxException ex) {
            throw new IllegalStateException("Unable to obtain settings file path");
        }

        try(Reader reader = Files.newBufferedReader(settingsPath)) {
            Gson gson = new Gson();
            Map<String, String> settings = gson.fromJson(reader, new TypeToken<Map<String, String>>() {}.getType());
            SETTINGS.putAll(settings);
        } catch(IOException ex) {
            throw new IllegalAccessError("Unable to read default settings");
        }
    }

    private final Map<String, String> properties;
    private final ExecutionTag executionTag;

    public Settings() {
        this.properties = new HashMap<>(SETTINGS);
        this.executionTag = new ExecutionTag();
        this.properties.put("executionTag", executionTag.toString());
    }

    public ExecutionTag getExecutionTag() {
        return executionTag;
    }

    public Path getOutputPath() {
        return Paths.get(getValue("executionOutputPath"));
    }

    public Settings put(String propertyName, String value) {
        SETTINGS.put(propertyName, value);
        return this;
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
        String value = getValueNoValidation(propertyName);
        if(value == null) {
            throw new IllegalArgumentException("Property not found: " + propertyName);
        }
        return value;
    }

    private String getValueNoValidation(String propertyName) {
        return properties.get(propertyName);
    }

    @Override
    public String toString() {
        return properties.toString();
    }
}
