package com.legadi.jurl.common;

import static com.legadi.jurl.common.Settings.mergeCredentialsToGlobal;
import static com.legadi.jurl.common.Settings.mergePropertiesToGlobal;
import static com.legadi.jurl.common.Settings.putPropertyToGlobal;
import static com.legadi.jurl.common.SettingsConstants.PROP_ENVIRONMENT;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import com.legadi.jurl.model.Credential;

public class SettingsSetter {

    private static final Logger LOGGER = Logger.getLogger(SettingsSetter.class.getName());

    private final Settings settings;
    private final boolean isGlobal;
    private final Map<String, String> priorityConfig;

    public SettingsSetter() {
        this(new Settings(), true);
    }

    public SettingsSetter(Settings settings) {
        this(settings, false);
    }

    private SettingsSetter(Settings settings, boolean isGlobal) {
        this.settings = settings;
        this.isGlobal = isGlobal;
        this.priorityConfig = new HashMap<>();
    }

    public Settings getSettings() {
        return settings;
    }

    public void putEnvironment(String value) {
        if(isGlobal) {
            putPropertyToGlobal(PROP_ENVIRONMENT, value);
        } else {
            settings.putOverride(PROP_ENVIRONMENT, value);
        }
    }

    public void put(String source, String property, String value) {
        if(PROP_ENVIRONMENT.equals(property)) {
            LOGGER.warning("[" + source + "] Property [" + PROP_ENVIRONMENT + "] can't be modified use [--env] option instead");
            return;
        }

        if(isGlobal) {
            putPropertyToGlobal(property, value);
        } else {
            settings.putOverride(property, value);
        }
    }

    public void putPriority(String source, String property, String value) {
        if(PROP_ENVIRONMENT.equals(property)) {
            LOGGER.warning("[" + source + "] Property [" + PROP_ENVIRONMENT + "] can't be modified use [--env] option instead");
            return;
        }

        priorityConfig.put(property, value);
    }

    public void mergeProperties(String source, Map<String, String> properties) {
        if(properties.containsKey(PROP_ENVIRONMENT)) {
            LOGGER.warning("[" + source + "] Property [" + PROP_ENVIRONMENT + "] can't be modified use [--env] option instead");
            properties.remove(PROP_ENVIRONMENT);
        }

        if(isGlobal) {
            mergePropertiesToGlobal(properties);
        } else {
            settings.mergeOverrideProperties(properties);
        }
    }

    public void mergeCredentials(Map<String, Credential> credentials) {
        if(isGlobal) {
            mergeCredentialsToGlobal(credentials);
        } else {
            settings.mergeOverrideCredentials(credentials);
        }
    }

    public void loadPriorityConfig() {
        if(isGlobal) {
            mergePropertiesToGlobal(priorityConfig);
        } else {
            settings.mergeOverrideProperties(priorityConfig);
        }
    }
}
