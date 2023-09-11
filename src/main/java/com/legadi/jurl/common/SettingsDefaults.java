package com.legadi.jurl.common;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Function;

import com.legadi.jurl.model.AuthorizationType;

import static com.legadi.jurl.common.SettingsConstants.*;

public interface SettingsDefaults {

    <T> T get(String propertyName, Function<String, T> mapper);

    String get(String propertyName);

    <T> T getOrDefault(String propertyName, T defaultValue, Function<String, T> mapper);

    String getOrDefault(String propertyName, String defaultValue);

    default Path getOutputPath() {
        return Paths.get(get(PROP_EXECUTION_OUTPUT_PATH));
    }

    default String[] getAddOnOptionClasses() {
        return getOrDefault(PROP_ADD_ON_OPTION_CLASSES, "").split(",");
    }

    default String getOpenEditorCommand() {
        return get(PROP_OPEN_EDITOR_COMMAND);
    }

    default String getCredentialId() {
        return get(PROP_REQUEST_CREDENTIAL_ID);
    }

    default AuthorizationType getAuthorizationType() {
        return AuthorizationType.valueOf(get(PROP_REQUEST_AUTHORIZATION_TYPE));
    }

    default boolean isExecutionAsFlow() {
        return get(PROP_EXECUTION_AS_FLOW, Boolean::valueOf);
    }

    default boolean isCurlRequest() {
        return get(PROP_CURL_REQUEST, Boolean::valueOf);
    }

    default boolean isMockRequest() {
        return get(PROP_MOCK_REQUEST, Boolean::valueOf);
    }

    default String getMockRequestClass() {
        return get(PROP_MOCK_REQUEST_CLASS);
    }

    default boolean isOpenOutputInEditor() {
        return get(PROP_OPEN_EDITOR_COMMAND, Boolean::valueOf);
    }

    default int getTimes() {
        return get(PROP_EXECUTION_TIMES, Integer::parseInt);
    }

    default String getInputName() {
        return get(PROP_INPUT_NAME);
    }

    default String getSettingsParamRegex() {
        return get(PROP_SETTINGS_PARAM_REGEX);
    }

    default String getSettingsParamRegexMask() {
        return get(PROP_SETTINGS_PARAM_REGEX_MASK);
    }

    default String getSettingsParamRegexReplace() {
        return get(PROP_SETTINGS_PARAM_REGEX_REPLACE);
    }

    default int getSettingsParamStartAt() {
        return get(PROP_SETTINGS_PARAM_START_AT, Integer::parseInt);
    }

    default int getSettingsParamEndAtLengthMinus() {
        return get(PROP_SETTINGS_PARAM_END_AT_LENGTH_MINUS, Integer::parseInt);
    }
}
