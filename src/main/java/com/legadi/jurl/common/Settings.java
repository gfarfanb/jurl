package com.legadi.jurl.common;

import static com.legadi.jurl.common.JsonUtils.loadInternalJsonProperties;
import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.LoaderUtils.loadCredentials;
import static com.legadi.jurl.common.SettingsConstants.PROP_CONFIG_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_OUTPUT_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_TAG;
import static com.legadi.jurl.common.SettingsConstants.PROP_WORKSPACE_PATH;
import static com.legadi.jurl.common.WriterUtils.createDirectories;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

public class Settings implements SettingsDefaults {

    private static final EnvironmentResource<String> SETTINGS = new EnvironmentResource<>();
    private static final EnvironmentResource<Credential> CREDENTIALS = new EnvironmentResource<>();
    private static final Map<String, String> EMPTY = new HashMap<>();

    public static final DateTimeFormatter TAG_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd.HH-mm-ss.n");
    public static final String DEFAULT_ENVIRONMENT = "default";

    private static final String DEFAULT_CONFIG_FILE = "config.json";
    private static final String DEFAULT_CREDENTIALS_FILE = "credentials.json";
    private static final String DEFAULT_OVERRIDE_FILE = "override.json";
    private static final String FORMAT_CONFIG_FILE = "config.%s.json";
    private static final String FORMAT_CREDENTIALS_FILE = "credentials.%s.json";
    private static final String FORMAT_OVERRIDE_FILE = "override.%s.json";

    static {
        SETTINGS.putAllInCommon(loadInternalJsonProperties("settings.default.json"));

        Path configPath = createDirectories(Paths.get(
            SETTINGS.get(DEFAULT_ENVIRONMENT, PROP_CONFIG_PATH)
        ));

        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadJsonProperties(configPath.resolve(DEFAULT_CONFIG_FILE)));
        CREDENTIALS.putAll(DEFAULT_ENVIRONMENT, loadCredentials(configPath.resolve(DEFAULT_CREDENTIALS_FILE)));

        Path executionOutputPath = createDirectories(Paths.get(
            SETTINGS.get(DEFAULT_ENVIRONMENT, PROP_EXECUTION_OUTPUT_PATH)
        ));

        SETTINGS.putAll(DEFAULT_ENVIRONMENT, loadJsonProperties(executionOutputPath.resolve(DEFAULT_OVERRIDE_FILE)));
    }

    private final Map<String, String> commandProperties;
    private final Map<String, String> userInputProperties;
    private final Map<String, String> overrideProperties;
    private final LocalDateTime timestamp;
    private final String executionTag;

    private String environment;

    public Settings() {
        this(DEFAULT_ENVIRONMENT, EMPTY, EMPTY);
    }

    private Settings(String environment,
            Map<String, String> userInputProperties,
            Map<String, String> overrideProperties) {
        this.environment = environment;
        this.commandProperties = new HashMap<>();
        this.userInputProperties = new HashMap<>(userInputProperties);
        this.overrideProperties = new HashMap<>(overrideProperties);

        this.timestamp = LocalDateTime.now();
        this.executionTag = TAG_FORMATTER.format(timestamp);

        commandProperties.put(PROP_EXECUTION_TAG, executionTag);
        commandProperties.put(PROP_WORKSPACE_PATH, getWorkspacePath().toString());
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
        return getOrDefaultWithValues(propertyName, EMPTY, defaultValue);
    }

    @Override
    public String getOrDefaultWithValues(String propertyName, Map<String, String> values,
            String defaultValue) {
        return commandProperties.getOrDefault(propertyName, 
                userInputProperties.getOrDefault(propertyName,
                    overrideProperties.getOrDefault(propertyName,
                        values.getOrDefault(propertyName,
                            SETTINGS.getOrDefault(environment, propertyName, defaultValue)
                        )
                    )
                )
            );
    }

    public Path getConfigFilePath() {
        return getFilePath(getConfigPath(), DEFAULT_CONFIG_FILE, FORMAT_CONFIG_FILE);
    }

    public Path getCredentialsFilePath() {
        return getFilePath(getConfigPath(), DEFAULT_CREDENTIALS_FILE, FORMAT_CREDENTIALS_FILE);
    }

    public Path getOverrideFilePath() {
        return getFilePath(getExecutionOutputPath(), DEFAULT_OVERRIDE_FILE, FORMAT_OVERRIDE_FILE);
    }

    public Path getOutputObjectPath() {
        return createDirectories(getExecutionOutputPath().resolve(getEnvironment()));
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
        return userInputProperties.containsKey(propertyName);
    }

    public void putUserInput(String propertyName, String propertyValue) {
        userInputProperties.put(propertyName, propertyValue);
    }

    public boolean containsOverride(String propertyName) {
        return overrideProperties.containsKey(propertyName);
    }

    public void putOverride(String propertyName, String propertyValue) {
        overrideProperties.put(propertyName, propertyValue);
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
        return new Settings(environment, userInputProperties, overrideProperties);
    }

    public Settings createForStep() {
        return new Settings(environment, userInputProperties, EMPTY);
    }

    public static void mergeProperties(String environment, Map<String, String> properties) {
        SETTINGS.putAll(environment, properties);
    }

    public static void mergeCommonProperties(Map<String, String> properties) {
        SETTINGS.putAllInCommon(properties);
    }

    public static void mergeCredentials(String environment, Map<String, Credential> credentials) {
        CREDENTIALS.putAll(environment, credentials);
    }
}
