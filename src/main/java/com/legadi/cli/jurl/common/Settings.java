package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.EMPTY_MAP;
import static com.legadi.cli.jurl.common.JsonUtils.loadInternalJsonProperties;
import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.SettingsConstants.DEFAULT_CONFIG_FILE;
import static com.legadi.cli.jurl.common.SettingsConstants.DEFAULT_ENVIRONMENT;
import static com.legadi.cli.jurl.common.SettingsConstants.DEFAULT_OVERRIDE_FILE;
import static com.legadi.cli.jurl.common.SettingsConstants.DEFAULT_SETTINGS_FILE;
import static com.legadi.cli.jurl.common.SettingsConstants.EXTERNAL_CONSOLE_WIDTH;
import static com.legadi.cli.jurl.common.SettingsConstants.EXTERNAL_OS_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.FORMAT_CONFIG_FILE;
import static com.legadi.cli.jurl.common.SettingsConstants.FORMAT_OVERRIDE_FILE;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_CONSOLE_WIDTH;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_FILE_SEPARATOR;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_OS_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_CONFIG_OUTPUT_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_CONFIG_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_EXECUTION_TAG;
import static com.legadi.cli.jurl.common.SettingsConstants.TAG_FORMATTER;
import static com.legadi.cli.jurl.common.WriterUtils.createDirectories;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import com.legadi.cli.jurl.exception.CommandException;

public class Settings implements SettingsDefaults {

    private static final EnvironmentResource<String> SETTINGS = new EnvironmentResource<>();

    static {
        System.setProperty(JURL_OS_NAME, System.getProperty(EXTERNAL_OS_NAME));
        System.setProperty(JURL_FILE_SEPARATOR, File.separator);
        System.setProperty(JURL_CONSOLE_WIDTH, Optional.ofNullable(System.getenv(EXTERNAL_CONSOLE_WIDTH))
            .orElse(""));

        SETTINGS.putAllInCommon(loadInternalJsonProperties(DEFAULT_SETTINGS_FILE));

        SETTINGS.putAllInCommon(loadJsonProperties(
            createDirectories(
                Paths.get(SETTINGS.get(null, PROP_CONFIG_PATH)))
                    .resolve(DEFAULT_CONFIG_FILE)));

        SETTINGS.putAllInCommon(loadJsonProperties(
            createDirectories(
                Paths.get(SETTINGS.get(null, PROP_CONFIG_OUTPUT_PATH)))
                    .resolve(DEFAULT_OVERRIDE_FILE)));
    }

    private final Map<String, String> userInputProperties;
    private final Map<String, String> overrideProperties;
    private final LocalDateTime timestamp;
    private final String executionTag;

    private String environment;

    public Settings() {
        this(DEFAULT_ENVIRONMENT, EMPTY_MAP, EMPTY_MAP);
    }

    private Settings(String environment,
            Map<String, String> userInputProperties,
            Map<String, String> overrideProperties) {
        this.environment = environment;
        this.userInputProperties = new HashMap<>(userInputProperties);
        this.overrideProperties = new HashMap<>(overrideProperties);

        this.timestamp = LocalDateTime.now();
        this.executionTag = TAG_FORMATTER.format(timestamp);

        userInputProperties.put(PROP_EXECUTION_TAG, executionTag);
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
        String value = getOrDefault(propertyName, null);
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
        return getOrDefaultWithValues(propertyName, EMPTY_MAP, defaultValue);
    }

    @Override
    public String getOrDefaultWithValues(String propertyName, Map<String, String> values,
            String defaultValue) {
        return userInputProperties.getOrDefault(propertyName,
                overrideProperties.getOrDefault(propertyName,
                    values.getOrDefault(propertyName,
                        SETTINGS.getOrDefault(environment, propertyName, defaultValue)
                    )
                )
            );
    }

    public Path getConfigFilePath() {
        return getFilePath(getConfigPath(), DEFAULT_CONFIG_FILE, FORMAT_CONFIG_FILE);
    }

    public Path getOverrideFilePath() {
        return getFilePath(getConfigOutputPath(), DEFAULT_OVERRIDE_FILE, FORMAT_OVERRIDE_FILE);
    }

    public Path getOutputObjectPath() {
        return createDirectories(getConfigOutputPath().resolve(getEnvironment()));
    }

    private Path getFilePath(Path basePath, String defaultFile, String formatFile) {
        String env = getEnvironment();

        if(DEFAULT_ENVIRONMENT.equals(env)) {
            return basePath.resolve(defaultFile);
        } else {
            return basePath.resolve(String.format(formatFile, env));
        }
    }

    public boolean containsUserInput(String propertyName) {
        return userInputProperties.get(propertyName) != null;
    }

    public void putUserInput(String propertyName, String propertyValue) {
        userInputProperties.put(propertyName, propertyValue);
    }

    public boolean containsOverride(String propertyName) {
        return overrideProperties.get(propertyName) != null;
    }

    public void putOverride(String propertyName, String propertyValue) {
        overrideProperties.put(propertyName, propertyValue);
    }

    public void removeProperties(String... properties) {
        Set<String> keys = new HashSet<>(Arrays.asList(properties));
        String env = getEnvironment();

        userInputProperties.keySet().removeAll(keys);
        overrideProperties.keySet().removeAll(keys);

        if(DEFAULT_ENVIRONMENT.equals(env)) {
            SETTINGS.removeAllInCommon(keys);
        } else {
            SETTINGS.removeAll(environment, keys);
        }
    }

    public Settings createForNextExecution() {
        return new Settings(environment, userInputProperties, overrideProperties);
    }

    public Settings createForStep() {
        return new Settings(environment, userInputProperties, EMPTY_MAP);
    }

    public static void mergeProperties(String environment, Map<String, String> properties) {
        SETTINGS.putAll(environment, properties);
    }
}
