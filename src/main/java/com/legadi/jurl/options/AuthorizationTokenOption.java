package com.legadi.jurl.options;

import static com.legadi.jurl.common.Settings.putProperty;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_CREDENTIAL_ID;
import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_AUTHORIZATION_TYPE;
import static com.legadi.jurl.model.AuthorizationType.TOKEN;

public class AuthorizationTokenOption extends Option {

    @Override
    public String getOpt() {
        return "--auth-token";
    }

    @Override
    public String getAlias() {
        return "-at";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "credential" };
    }

    @Override
    public String getDescription() {
        return "Assigns 'credential' to the request.\nTells 'request' to use TOKEN authentication.";
    }

    @Override
    public boolean execute(String[] args) {
        putProperty(PROP_REQUEST_CREDENTIAL_ID, args[0]);
        putProperty(PROP_REQUEST_AUTHORIZATION_TYPE, TOKEN.name());
        return true;
    }
}
