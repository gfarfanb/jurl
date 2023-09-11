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

public class Settings implements SettingsDefaults {

    private static final EnvironmentResource<String> SETTINGS = new EnvironmentResource<>();
    private static final EnvironmentResource<Credential> CREDENTIALS = new EnvironmentResource<>();

    private static final String DEFAULT_ENVIRONMENT = "default";
    private static final String DEFAULT_CONFIG_FILE = "./config.json";
    private static final String DEFAULT_OVERRIDE_FILE = "./.override.json";
    private static final String DEFAULT_CREDENTIALS_FILE = "./credentials.json";
    private static final String FORMAT_CONFIG_FILE = "./config.%s.json";
    private static final String FORMAT_OVERRIDE_FILE = "./.override.%s.json";
    private static final String FORMAT_CREDENTIALS_FILE = "./credentials.%s.json";

    static {
        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadInternalJsonProperties("settings.default.json", true));
        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadJsonProperties(DEFAULT_CONFIG_FILE, true));
        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadJsonProperties(DEFAULT_OVERRIDE_FILE, true));
        CREDENTIALS.putAll(DEFAULT_ENVIRONMENT, loadCredentials(DEFAULT_CREDENTIALS_FILE, true));
    }

    private final Map<String, String> overrideProperties;
    private final LocalDateTime timestamp;
    private final String executionTag;

    private String environment;

    public Settings() {
        this(DEFAULT_ENVIRONMENT, new HashMap<>());
    }

    private Settings(String environment, Map<String, String> overrideProperties) {
        this.overrideProperties = new HashMap<>(overrideProperties);
        this.timestamp = LocalDateTime.now();
        this.executionTag = timestamp.toLocalDate() + "."  + timestamp.toLocalTime().getLong(MILLI_OF_DAY);
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }
    
    public String getExecutionTag() {
        return executionTag;
    }

    public String getEnvironment() {
        return environment;
    }

    public void setEnvironment(String environment) {
        this.environment = environment;
    }

    @Override
    public <T> T get(String propertyName, Function<String, T> mapper) {
        return mapper.apply(get(propertyName));
    }

    @Override
    public String get(String propertyName) {
        String value = overrideProperties.getOrDefault(propertyName,
            SETTINGS.get(environment, propertyName));
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
        return overrideProperties.getOrDefault(propertyName,
            SETTINGS.getOrDefault(environment, propertyName, defaultValue));
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

    public void putOverride(String propertyName, String propertyValue) {
        this.overrideProperties.put(propertyName, propertyValue);
    }

    public void mergeOverrideProperties(Map<String, String> properties) {
        this.overrideProperties.putAll(properties);
    }

    public Credential getCredential() {
        String credentialId = getCredentialId();
        Credential credential = CREDENTIALS.get(environment, credentialId);
        if(credential == null) {
            throw new CommandException("Credential not found: " + credentialId);
        }
        return credential;
    }

    public Settings createForNextExecution() {
        return new Settings(environment, overrideProperties);
    }

    public static void mergeProperties(String environment, Map<String, String> properties) {
        SETTINGS.putAll(environment, properties);
    }

    public static void mergeCredentials(String environment, Map<String, Credential> credentials) {
        CREDENTIALS.putAll(environment, credentials);
    }
}
