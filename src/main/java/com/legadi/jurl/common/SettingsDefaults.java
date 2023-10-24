package com.legadi.jurl.common;

import static com.legadi.jurl.common.SettingsConstants.PROP_ADD_ON_OPTION_CLASSES;
import static com.legadi.jurl.common.SettingsConstants.PROP_CONFIG_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_AS_FLOW;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_OUTPUT_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_TIMES;
import static com.legadi.jurl.common.SettingsConstants.PROP_HISTORY_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_INPUT_NAME;
import static com.legadi.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;
import static com.legadi.jurl.common.SettingsConstants.PROP_MOCK_REQUEST;
import static com.legadi.jurl.common.SettingsConstants.PROP_OPEN_EDITOR_COMMAND;
import static com.legadi.jurl.common.SettingsConstants.PROP_OVERRIDE_REQUEST_FILE_PATH;
import static com.legadi.jurl.common.SettingsConstants.PROP_PRINTABLE_MIME_TYPES;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_AUTHORIZATION_TYPE;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_CREDENTIAL_ID;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_TYPE;
import static com.legadi.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_END_AT_LENGTH_MINUS;
import static com.legadi.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_REGEX;
import static com.legadi.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_REGEX_MASK;
import static com.legadi.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_REGEX_REPLACE;
import static com.legadi.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_START_AT;
import static com.legadi.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;
import static com.legadi.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;
import static com.legadi.jurl.common.WriterUtils.createDirectories;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Function;

import com.legadi.jurl.model.AuthorizationType;
import com.legadi.jurl.model.RequestBehaviour;

public interface SettingsDefaults {

    <T> T get(String propertyName, Function<String, T> mapper);

    String get(String propertyName);

    <T> T getOrDefault(String propertyName, T defaultValue, Function<String, T> mapper);

    String getOrDefault(String propertyName, String defaultValue);

    default Path getConfigPath() {
        return createDirectories(Paths.get(get(PROP_CONFIG_PATH)));
    }

    default Path getExecutionPath() {
        return createDirectories(Paths.get(get(PROP_EXECUTION_PATH)));
    }

    default Path getExecutionOutputPath() {
        return createDirectories(Paths.get(get(PROP_EXECUTION_OUTPUT_PATH)));
    }

    default Path getHistoryPath() {
        return createDirectories(Paths.get(get(PROP_HISTORY_PATH)));
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

    default RequestBehaviour getRequestBehaviour() {
        return get(PROP_REQUEST_BEHAVIOUR, RequestBehaviour::valueOf);
    }

    default String getRequestType() {
        return get(PROP_REQUEST_TYPE);
    }

    default boolean isMockRequest() {
        return get(PROP_MOCK_REQUEST, Boolean::valueOf);
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

    default boolean isSkipConditions() {
        return get(PROP_SKIP_CONDITIONS, Boolean::valueOf);
    }

    default boolean isSkipAssertions() {
        return get(PROP_SKIP_ASSERTIONS, Boolean::valueOf);
    }

    default String getMergeBodyUsingType() {
        return get(PROP_MERGE_BODY_USING_TYPE);
    }

    default String getOverrideRequestFilePath() {
        return get(PROP_OVERRIDE_REQUEST_FILE_PATH);
    }

    default String[] getPrintableMimeTypes() {
        return getOrDefault(PROP_PRINTABLE_MIME_TYPES, "").split(",");
    }
}
