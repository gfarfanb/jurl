package com.legadi.jurl.common;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.time.temporal.ChronoField.MILLI_OF_DAY;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

import static com.legadi.jurl.common.Loader.loadCredentials;
import static com.legadi.jurl.common.Loader.loadInternalJsonProperties;
import static com.legadi.jurl.common.Loader.loadJsonProperties;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_TAG;

public class Settings implements SettingsDefaults {

    private static final Map<String, String> SETTINGS = new HashMap<>();
    private static final Map<String, Credential> CREDENTIALS = new HashMap<>();

    static {
        SETTINGS.putAll(loadInternalJsonProperties("settings.default.json", true));
        SETTINGS.putAll(loadJsonProperties("./config.json", true));
        CREDENTIALS.putAll(loadCredentials("./credentials.json", true));
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

    public void put(String propertyName, String propertyValue) {
        this.properties.put(propertyName, propertyValue);
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

    public String replaceAllInContent(String content) {
        Pattern pattern = Pattern.compile(getSettingsParamRegex());
        Matcher matcher = pattern.matcher(content);
        Set<String> paramTags = new HashSet<>();

        while(matcher.find()) {
            String paramTag = matcher.group(0);
            
            if(!paramTags.contains(paramTag)) {
                String paramName = paramTag.substring(
                    getSettingsParamStartAt(),
                    paramTag.length() - getSettingsParamEndAtLengthMinus()
                );
                String paramRegex = getSettingsParamRegexMask().replace(getSettingsParamRegexReplace(), paramName);
                
                content = content.replaceAll(paramRegex, get(paramName));

                paramTags.add(paramTag);
            }
        }

        return content;
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
