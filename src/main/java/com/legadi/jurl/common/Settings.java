package com.legadi.jurl.common;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.AuthorizationType;
import com.legadi.jurl.model.Credential;

import static com.legadi.jurl.common.Loader.loadCredentials;
import static com.legadi.jurl.common.Loader.loadInternalJsonProperties;
import static com.legadi.jurl.common.Loader.loadJsonProperties;
import static com.legadi.jurl.common.SettingsConstants.*;
import static com.legadi.jurl.common.StringUtils.isNotBlank;

public class Settings {

    private static final Map<String, String> SETTINGS = new HashMap<>();
    private static final Map<String, String> OVERRIDE_SETTINGS = new HashMap<>();
    private static final Map<String, Credential> CREDENTIALS = new HashMap<>();
    private static final Map<String, Credential> OVERRIDE_CREDENTIALS = new HashMap<>();

    static {
        SETTINGS.putAll(loadInternalJsonProperties("settings.default.json", true));
        SETTINGS.putAll(loadJsonProperties("./config.json", true));
        CREDENTIALS.putAll(loadCredentials("./credentials.json", true));
    }

    private final Map<String, String> properties;
    private final Map<String, Credential> credentials;
    private final ExecutionTag executionTag;

    public Settings() {
        this.properties = new HashMap<>(SETTINGS);
        this.credentials = new HashMap<>(CREDENTIALS);
        this.executionTag = new ExecutionTag();
        this.properties.put(PROP_EXECUTION_TAG, executionTag.toString());

        this.properties.putAll(OVERRIDE_SETTINGS);
        this.credentials.putAll(OVERRIDE_CREDENTIALS);
    }

    public ExecutionTag getExecutionTag() {
        return executionTag;
    }

    public Path getOutputPath() {
        return Paths.get(getValue(PROP_EXECUTION_OUTPUT_PATH));
    }

    public String[] getAddOnOptionClasses() {
        return get(PROP_ADD_ON_OPTION_CLASSES, "").split(",");
    }

    public String getOpenEditorCommand() {
        return getValue(PROP_OPEN_EDITOR_COMMAND);
    }

    public Credential getCredential() {
        String credentialId = getValue(PROP_REQUEST_CREDENTIAL_ID);
        Credential credential = credentials.get(credentialId);
        if(credential == null) {
            throw new CommandException("Credential not found [" + PROP_REQUEST_CREDENTIAL_ID + "]: " + credentialId);
        }
        return credential;
    }

    public AuthorizationType getAuthorizationType() {
        return AuthorizationType.valueOf(getValue(PROP_REQUEST_AUTHORIZATION_TYPE));
    }

    public boolean isCurlRequest() {
        return get(PROP_CURL_REQUEST, Boolean::valueOf);
    }

    public boolean isMockRequest() {
        return get(PROP_MOCK_REQUEST, Boolean::valueOf);
    }

    public String getMockRequestClass() {
        return getValue(PROP_MOCK_REQUEST_CLASS);
    }

    public boolean isOpenOutputInEditor() {
        return get(PROP_OPEN_EDITOR_COMMAND, Boolean::valueOf);
    }

    public int getTimes() {
        return get(PROP_EXECUTION_TIMES, Integer::parseInt);
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
            throw new CommandException("Property not found: " + propertyName);
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

    public static void mergeProperties(Map<String, String> properties) {
        OVERRIDE_SETTINGS.putAll(properties);
    }

    public static void putProperty(String propertyName, String propertyValue) {
        OVERRIDE_SETTINGS.put(propertyName, propertyValue);
    }

    public static void mergeCredentials(Map<String, Credential> credentials) {
        String credentialIdFound = credentials.keySet()
            .stream()
            .filter(CREDENTIALS::containsKey)
            .findFirst()
            .orElse(null);

        if(isNotBlank(credentialIdFound)) {
            throw new CommandException("Credential ID already exists: " + credentialIdFound);
        } else {
            CREDENTIALS.putAll(credentials);
        }
    }

    public static void clearSettings() {
        OVERRIDE_SETTINGS.clear();
        OVERRIDE_CREDENTIALS.clear();
    }
}
