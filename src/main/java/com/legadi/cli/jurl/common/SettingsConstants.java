package com.legadi.cli.jurl.common;

import java.time.format.DateTimeFormatter;

public class SettingsConstants {

    public static final DateTimeFormatter TAG_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd.HH-mm-ss.n");
    public static final String DEFAULT_ENVIRONMENT = "default";

    public static final String DEFAULT_SETTINGS_FILE = "settings.default.json";
    public static final String DEFAULT_CONFIG_FILE = "config.json";
    public static final String DEFAULT_OVERRIDE_FILE = "override.json";
    public static final String FORMAT_CONFIG_FILE = "config.%s.json";
    public static final String FORMAT_OVERRIDE_FILE = "override.%s.json";

    public static final String EXTERNAL_OS_NAME = "os.name";
    public static final String JURL_OS_NAME = "jurl.os.name";
    public static final String JURL_FILE_SEPARATOR = "jurl.file.separator";
    public static final String EXTERNAL_CONSOLE_WIDTH = "JURL_CONSOLE_WIDTH";
    public static final String JURL_CONSOLE_WIDTH = "jurl.console.width";

    public static final String PROP_EXECUTION_TAG = "executionTag";
    public static final String PROP_WORKSPACE_PATH = "workspacePath";
    public static final String PROP_CONFIG_PATH = "configPath";
    public static final String PROP_CONFIG_OUTPUT_PATH = "configOutputPath";
    public static final String PROP_EXECUTION_PATH = "executionPath";
    public static final String PROP_HISTORY_PATH = "historyPath";
    public static final String PROP_ADD_ON_OPTION_CLASSES = "addOnOptionClasses";
    public static final String PROP_OPEN_EDITOR_COMMAND = "openEditorCommand";
    public static final String PROP_DOWNLOADS_LOCATION = "downloadsLocation";
    public static final String PROP_REQUEST_BEHAVIOUR = "requestBehaviour";
    public static final String PROP_REQUEST_TYPE = "requestType";
    public static final String PROP_MOCK_REQUEST = "mockRequest";
    public static final String PROP_OPEN_OUTPUT_IN_EDITOR = "openOutputInEditor";
    public static final String PROP_SAVE_OUTPUT_IN_LOCATION = "saveOutputInLocation";
    public static final String PROP_EXECUTION_TIMES = "executionTimes";
    public static final String PROP_INPUT_NAME = "inputName";
    public static final String PROP_FILTER_NAME = "filterName";
    public static final String PROP_START_IN_STEP_INDEX = "startInStepIndex";
    public static final String PROP_SKIP_USER_INPUT = "skipUserInput";
    public static final String PROP_SETTINGS_PARAM_REGEX = "settingsParamRegex";
    public static final String PROP_SETTINGS_PARAM_REGEX_BEGIN = "settingsParamRegexBegin";
    public static final String PROP_SETTINGS_PARAM_REGEX_END = "settingsParamRegexEnd";
    public static final String PROP_SKIP_AUTHENTICATION = "skipAuthentication";
    public static final String PROP_SKIP_CONDITIONS = "skipConditions";
    public static final String PROP_SKIP_ASSERTIONS = "skipAssertions";
    public static final String PROP_MERGE_BODY_USING_TYPE = "mergeBodyUsingType";
    public static final String PROP_OVERRIDE_REQUEST_FILE_PATH = "overrideRequestFilePath";
    public static final String PROP_PRINTABLE_MIME_TYPES = "printableMimeTypes";
    public static final String PROP_CONSOLE_TAB_LENGTH = "consoleTabLength";
    public static final String PROP_DEFAULT_CONSOLE_WIDTH = "defaultConsoleWidth";
    public static final String PROP_AUTH_BEARER_GRANT_TYPE = "authBearerGrantType";
    public static final String PROP_AUTH_BEARER_GRANT_TYPE_FIELD_NAME = "authBearerGrantTypeFieldName";
    public static final String PROP_AUTH_BEARER_CLIENT_ID_FIELD_NAME = "authBearerClientIdFieldName";
    public static final String PROP_AUTH_BEARER_CLIENT_SECRET_FIELD_NAME = "authBearerClientSecretFieldName";
    public static final String PROP_AUTH_BEARER_SCOPE_FIELD_NAME = "authBearerScopeFieldName";
    public static final String PROP_AUTH_BEARER_ACCESS_TOKEN_FIELD_NAME = "authBearerAccessTokenFieldName";
    public static final String PROP_AUTH_BEARER_EXPIRES_IN_FIELD_NAME = "authBearerExpiresInFieldName";
    public static final String PROP_AUTH_BEARER_EXPIRES_IN_TIME_UNIT = "authBearerExpiresInTimeUnit";
    public static final String PROP_AUTH_BEARER_TOKEN_TYPE_FIELD_NAME = "authBearerTokenTypeFieldName";
    public static final String PROP_AUTH_BEARER_REQUEST_METHOD = "authBearerRequestMethod";
    public static final String PROP_AUTH_BEARER_CONTENT_TYPE = "authBearerContentType";
    public static final String PROP_AUTH_BEARER_BODY_TEMPLATE = "authBearerBodyTemplate";

    private SettingsConstants() {}
}
