package com.legadi.jurl.common;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import static java.time.temporal.ChronoField.MILLI_OF_DAY;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

import static com.legadi.jurl.common.LoaderUtils.loadCredentials;
import static com.legadi.jurl.common.LoaderUtils.loadInternalJsonProperties;
import static com.legadi.jurl.common.LoaderUtils.loadJsonProperties;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_TAG;

public class Settings implements SettingsDefaults {

    private static final Map<String, String> SETTINGS = new HashMap<>();
    private static final Map<String, Credential> CREDENTIALS = new HashMap<>();

    private static final String DEFAULT_ENVIRONMENT = "default";
    private static final String DEFAULT_CONFIG_FILE = "./config.json";
    private static final String DEFAULT_OVERRIDE_FILE = "./.override.json";
    private static final String DEFAULT_CREDENTIALS_FILE = "./credentials.json";
    private static final String FORMAT_CONFIG_FILE = "./config.%s.json";
    private static final String FORMAT_OVERRIDE_FILE = "./.override.%s.json";
    private static final String FORMAT_CREDENTIALS_FILE = "./credentials.%s.json";

    static {
        SETTINGS.putAll(loadInternalJsonProperties("settings.default.json", true));
        SETTINGS.putAll(loadJsonProperties(DEFAULT_CONFIG_FILE, true));
        SETTINGS.putAll(loadJsonProperties(DEFAULT_OVERRIDE_FILE, true));
        CREDENTIALS.putAll(loadCredentials(DEFAULT_CREDENTIALS_FILE, true));
    }

    private final Map<String, String> properties;
    private final Map<String, String> overrideProperties;
    private final Map<String, Credential> credentials;
    private final Map<String, Credential> overrideCredentials;
    private final LocalDateTime timestamp;

    public Settings() {
        this(SETTINGS, CREDENTIALS);
    }

    private Settings(Map<String, String> properties, Map<String, Credential> credentials) {
        this.properties = new HashMap<>(properties);
        this.overrideProperties = new HashMap<>();
        this.credentials = new HashMap<>(credentials);
        this.overrideCredentials = new HashMap<>();
        this.timestamp = LocalDateTime.now();

        this.properties.put(PROP_EXECUTION_TAG,
            timestamp.toLocalDate() + "."  + timestamp.toLocalTime().getLong(MILLI_OF_DAY));
    }

    @Override
    public <T> T get(String propertyName, Function<String, T> mapper) {
        return mapper.apply(get(propertyName));
    }

    @Override
    public String get(String propertyName) {
        String value = overrideProperties.getOrDefault(propertyName, properties.get(propertyName));
        if(value == null) {
            throw new CommandException("Property not found: " + propertyName);
        }
        return value;
    }

    @Override
    public <T> T getOrDefault(String propertyName, T defaultValue, Function<String, T> mapper) {
        String value = getOrDefault(propertyName, null);
        if(value == null) {
            return defaultValue;
        } else {
            return mapper.apply(value);
        }
    }

    @Override
    public String getOrDefault(String propertyName, String defaultValue) {
        return overrideProperties.getOrDefault(propertyName, properties.getOrDefault(propertyName, defaultValue));
    }

    public String getConfigFileName() {
        String env = getEnvironment();

        if(DEFAULT_ENVIRONMENT.equals(env)) {
            return DEFAULT_CONFIG_FILE;
        } else {
            return String.format(FORMAT_CONFIG_FILE, env);
        }
    }

    public String getOverrideFileName() {
        String env = getEnvironment();

        if(DEFAULT_ENVIRONMENT.equals(env)) {
            return DEFAULT_OVERRIDE_FILE;
        } else {
            return String.format(FORMAT_OVERRIDE_FILE, env);
        }
    }

    public String getCredentialsFileName() {
        String env = getEnvironment();

        if(DEFAULT_ENVIRONMENT.equals(env)) {
            return DEFAULT_CREDENTIALS_FILE;
        } else {
            return String.format(FORMAT_CREDENTIALS_FILE, env);
        }
    }

    public void putProperties(Map<String, String> properties) {
        this.properties.putAll(properties);
    }

    public void putOverride(String propertyName, String propertyValue) {
        this.overrideProperties.put(propertyName, propertyValue);
    }

    public void mergeOverrideProperties(Map<String, String> properties) {
        this.overrideProperties.putAll(properties);
    }

    public void mergeOverrideCredentials(Map<String, Credential> credentials) {
        this.overrideCredentials.putAll(credentials);
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public Credential getCredential() {
        String credentialId = getCredentialId();
        Credential credential = overrideCredentials.getOrDefault(credentialId, credentials.get(credentialId));
        if(credential == null) {
            throw new CommandException("Credential not found: " + credentialId);
        }
        return credential;
    }

    public Settings createForNextExecution() {
        return new Settings(properties, credentials);
    }

    @Override
    public String toString() {
        return properties.toString();
    }

    public static void putPropertyToGlobal(String propertyName, String propertyValue) {
        SETTINGS.put(propertyName, propertyValue);
    }

    public static void mergePropertiesToGlobal(Map<String, String> properties) {
        SETTINGS.putAll(properties);
    }

    public static void mergeCredentialsToGlobal(Map<String, Credential> credentials) {
        CREDENTIALS.putAll(credentials);
    }
}
