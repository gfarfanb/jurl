package com.legadi.jurl.common;

import static com.legadi.jurl.common.LoaderUtils.loadCredentials;
import static com.legadi.jurl.common.LoaderUtils.loadInternalJsonProperties;
import static com.legadi.jurl.common.LoaderUtils.loadJsonProperties;
import static com.legadi.jurl.common.CommonUtils.createDirectories;
import static com.legadi.jurl.common.SettingsConstants.PROP_CONFIG_PATH;
import static java.time.temporal.ChronoField.MILLI_OF_DAY;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

public class Settings implements SettingsDefaults {

    private static final EnvironmentResource<String> SETTINGS = new EnvironmentResource<>();
    private static final EnvironmentResource<Credential> CREDENTIALS = new EnvironmentResource<>();

    public static final String DEFAULT_ENVIRONMENT = "default";

    private static final String DEFAULT_CONFIG_FILE = "config.json";
    private static final String DEFAULT_OVERRIDE_FILE = ".override.json";
    private static final String DEFAULT_CREDENTIALS_FILE = "credentials.json";
    private static final String FORMAT_CONFIG_FILE = "config.%s.json";
    private static final String FORMAT_OVERRIDE_FILE = ".override.%s.json";
    private static final String FORMAT_CREDENTIALS_FILE = "credentials.%s.json";

    static {
        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadInternalJsonProperties("settings.default.json"));

        Path configPath = createDirectories(Paths.get(SETTINGS.get(DEFAULT_ENVIRONMENT, PROP_CONFIG_PATH)));

        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadJsonProperties(configPath.resolve(DEFAULT_CONFIG_FILE)));
        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadJsonProperties(configPath.resolve(DEFAULT_OVERRIDE_FILE)));
        CREDENTIALS.putAll(DEFAULT_ENVIRONMENT, loadCredentials(configPath.resolve(DEFAULT_CREDENTIALS_FILE)));
    }

    private final Map<String, String> overrideProperties;
    private final LocalDateTime timestamp;
    private final String executionTag;

    private String environment;

    public Settings() {
        this(DEFAULT_ENVIRONMENT, new HashMap<>());
    }

    private Settings(String environment, Map<String, String> overrideProperties) {
        this.environment = environment;
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

    public Path getConfigFilePath() {
        return getConfigFile(DEFAULT_CONFIG_FILE, FORMAT_CONFIG_FILE);
    }

    public Path getOverrideFilePath() {
        return getConfigFile(DEFAULT_OVERRIDE_FILE, FORMAT_OVERRIDE_FILE);
    }

    public Path getCredentialsFilePath() {
        return getConfigFile(DEFAULT_CREDENTIALS_FILE, FORMAT_CREDENTIALS_FILE);
    }

    private Path getConfigFile(String defaultFile, String formatFile) {
        String env = getEnvironment();

        if(DEFAULT_ENVIRONMENT.equals(env)) {
            return getConfigPath().resolve(defaultFile);
        } else {
            return getConfigPath().resolve(String.format(formatFile, env));
        }
    }

    public boolean containsOverride(String propertyName) {
        return overrideProperties.containsKey(propertyName);
    }

    public void putOverride(String propertyName, String propertyValue) {
        overrideProperties.put(propertyName, propertyValue);
    }

    public void mergeOverrideProperties(Map<String, String> properties) {
        overrideProperties.putAll(properties);
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
