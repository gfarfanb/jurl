package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_CREDENTIAL_ID;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_AUTHORIZATION_TYPE;
import static com.legadi.jurl.model.AuthorizationType.BASIC;

import com.legadi.jurl.common.SettingsSetter;

public class AuthorizationBasicOption extends Option {

    @Override
    public String getOpt() {
        return "--auth-basic";
    }

    @Override
    public String getAlias() {
        return "-ab";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "credential" };
    }

    @Override
    public String getDescription() {
        return "Assigns 'credential' to the request.\nTells 'request' to use BASIC authentication.";
    }

    @Override
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.put(getOpt(), PROP_REQUEST_CREDENTIAL_ID, args[0]);
        settings.put(getOpt(), PROP_REQUEST_AUTHORIZATION_TYPE, BASIC.name());
        return true;
    }
}
