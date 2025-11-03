package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_AUTHENTICATION;

import com.legadi.cli.jurl.common.Settings;

public class UseAuthenticationOption extends Option {

    @Override
    public String name() {
        return "--use-auth";
    }

    @Override
    public String alias() {
        return "-ua";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to use the authentication request.";
    }

    @Override
    public boolean requiredForAuth() {
        return true;
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_SKIP_AUTHENTICATION, Boolean.FALSE.toString());
        return true;
    }
}
