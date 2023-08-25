package com.legadi.jurl.common;

import static com.legadi.jurl.common.Settings.mergeCredentialsToGlobal;
import static com.legadi.jurl.common.Settings.mergePropertiesToGlobal;
import static com.legadi.jurl.common.Settings.putPropertyToGlobal;

import java.util.Map;

import com.legadi.jurl.model.Credential;

public class SettingsSetter {

    private final Settings settings;

    public SettingsSetter() {
        this(null);
    }

    public SettingsSetter(Settings settings) {
        this.settings = settings;
    }

    public void put(String property, String value) {
        if(settings == null) {
            putPropertyToGlobal(property, value);
        } else {
            settings.putOverride(property, value);
        }
    }

    public void mergeProperties(Map<String, String> properties) {
        if(settings == null) {
            mergePropertiesToGlobal(properties);
        } else {
            settings.mergeOverrideProperties(properties);
        }
    }

    public void mergeCredentials(Map<String, Credential> credentials) {
        if(settings == null) {
            mergeCredentialsToGlobal(credentials);
        } else {
            settings.mergeOverrideCredentials(credentials);
        }
    }
}
