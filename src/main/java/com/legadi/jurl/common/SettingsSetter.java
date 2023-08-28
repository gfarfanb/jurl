package com.legadi.jurl.common;

import static com.legadi.jurl.common.Settings.mergeCredentialsToGlobal;
import static com.legadi.jurl.common.Settings.mergePropertiesToGlobal;
import static com.legadi.jurl.common.Settings.putPropertyToGlobal;

import java.util.HashMap;
import java.util.Map;

import com.legadi.jurl.model.Credential;

public class SettingsSetter {

    private final Settings settings;
    private final boolean isGlobal;
    private final Map<String, String> priorityConfig;

    public SettingsSetter() {
        this(null, true);
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

    public void put(String property, String value) {
        if(isGlobal) {
            putPropertyToGlobal(property, value);
        } else {
            settings.putOverride(property, value);
        }
    }

    public void putPriority(String property, String value) {
        priorityConfig.put(property, value);
    }

    public void mergeProperties(Map<String, String> properties) {
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
