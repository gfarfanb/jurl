package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.SettingsConstants.JURL_CONSOLE_WIDTH;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_FILE_SEPARATOR;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_OS_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_ADD_ON_OPTION_CLASSES;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_CONFIG_OUTPUT_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_CONFIG_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_CONSOLE_TAB_LENGTH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_DEFAULT_CONSOLE_WIDTH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_DOWNLOADS_LOCATION;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_EXECUTE_AUTHENTICATION;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_EXECUTION_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_EXECUTION_TIMES;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_FILTER_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_HISTORY_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_INPUT_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MOCK_REQUEST;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OPEN_EDITOR_COMMAND;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OPEN_OUTPUT_IN_EDITOR;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OVERRIDE_REQUEST_FILE_PATH;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_PRINTABLE_MIME_TYPES;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_TYPE;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SAVE_OUTPUT_IN_LOCATION;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_REGEX;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_REGEX_BEGIN;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SETTINGS_PARAM_REGEX_END;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_AUTHENTICATION;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_USER_INPUT;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_START_IN_STEP_INDEX_OR_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_WORKSPACE_PATH;
import static com.legadi.cli.jurl.common.WriterUtils.createDirectories;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import com.legadi.cli.jurl.model.RequestBehaviour;

public interface SettingsDefaults {

    <T> T get(String propertyName, Function<String, T> mapper);

    String get(String propertyName);

    <T> T getOrDefault(String propertyName, T defaultValue, Function<String, T> mapper);

    String getOrDefault(String propertyName, String defaultValue);

    String getOrDefaultWithValues(String propertyName, Map<String, String> values,
            String defaultValue);

    default String getOSName() {
        return System.getProperty(JURL_OS_NAME);
    }

    default boolean isWindowsOS() {
        return Optional.ofNullable(getOSName())
            .map(String::toLowerCase)
            .filter(os -> os.contains("win"))
            .isPresent();
    }

    default String getFileSeparator() {
        return System.getProperty(JURL_FILE_SEPARATOR);
    }

    default int getConsoleWidth() {
        return Optional.ofNullable(System.getProperty(JURL_CONSOLE_WIDTH))
            .filter(CommonUtils::isNumeric)
            .map(Integer::parseInt)
            .orElse(getDefaultConsoleWidth());
    }

    default Path getWorkspacePath() {
        return Paths.get(get(PROP_WORKSPACE_PATH)).toAbsolutePath().normalize();
    }

    default Path getConfigPath() {
        return createDirectories(Paths.get(get(PROP_CONFIG_PATH)));
    }

    default Path getConfigOutputPath() {
        return createDirectories(Paths.get(get(PROP_CONFIG_OUTPUT_PATH)));
    }

    default Path getExecutionPath() {
        return createDirectories(Paths.get(get(PROP_EXECUTION_PATH)));
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

    default String getDownloadsLocation() {
        return get(PROP_DOWNLOADS_LOCATION);
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
        return get(PROP_OPEN_OUTPUT_IN_EDITOR, Boolean::valueOf);
    }

    default boolean isSaveOutputInLocation() {
        return get(PROP_SAVE_OUTPUT_IN_LOCATION, Boolean::valueOf);
    }

    default int getTimes() {
        return get(PROP_EXECUTION_TIMES, Integer::parseInt);
    }

    default String getInputName() {
        return get(PROP_INPUT_NAME);
    }

    default String getFilterName() {
        return get(PROP_FILTER_NAME);
    }

    default String getStartInStepIndexOrName() {
        return get(PROP_START_IN_STEP_INDEX_OR_NAME);
    }

    default boolean isSkipUserInput() {
        return get(PROP_SKIP_USER_INPUT, Boolean::valueOf);
    }

    default String getSettingsParamRegex() {
        return get(PROP_SETTINGS_PARAM_REGEX);
    }

    default String getSettingsParamRegexBegin() {
        return get(PROP_SETTINGS_PARAM_REGEX_BEGIN);
    }

    default String getSettingsParamRegexEnd() {
        return get(PROP_SETTINGS_PARAM_REGEX_END);
    }

    default boolean isExecuteAuthentication() {
        return get(PROP_EXECUTE_AUTHENTICATION, Boolean::valueOf);
    }

    default boolean isSkipAuthentication() {
        return get(PROP_SKIP_AUTHENTICATION, Boolean::valueOf);
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
        return Arrays.asList(getOrDefault(PROP_PRINTABLE_MIME_TYPES, "").split(","))
            .stream()
            .map(String::trim)
            .toArray(String[]::new);
    }

    default int getConsoleTabLength() {
        return get(PROP_CONSOLE_TAB_LENGTH, Integer::parseInt);
    }

    default int getDefaultConsoleWidth() {
        return get(PROP_DEFAULT_CONSOLE_WIDTH, Integer::parseInt);
    }

    default String getTab() {
        return String.format("%-" + getConsoleTabLength() + "s", "");
    }
}
