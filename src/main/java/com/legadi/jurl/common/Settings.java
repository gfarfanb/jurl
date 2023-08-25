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
import static com.legadi.jurl.common.StringUtils.isNotBlank;

public class Settings implements SettingsDefaults {

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
    private final LocalDateTime timestamp;

    public Settings() {
        this.properties = new HashMap<>(SETTINGS);
        this.credentials = new HashMap<>(CREDENTIALS);
        this.timestamp = LocalDateTime.now();

        this.properties.put(PROP_EXECUTION_TAG,
            timestamp.toLocalDate() + "."  + timestamp.toLocalTime().getLong(MILLI_OF_DAY));
        this.properties.putAll(OVERRIDE_SETTINGS);
        this.credentials.putAll(OVERRIDE_CREDENTIALS);
    }

    @Override
    public <T> T get(String propertyName, Function<String, T> mapper) {
        return mapper.apply(get(propertyName));
    }

    @Override
    public String get(String propertyName) {
        String value = properties.get(propertyName);
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
        return properties.getOrDefault(propertyName, defaultValue);
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public Credential getCredential() {
        String credentialId = getCredentialId();
        Credential credential = credentials.get(credentialId);
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
}
