package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_AUTHENTICATION;

import com.legadi.cli.jurl.common.Settings;

public class SkipAuthenticationOption extends Option {

    @Override
    public String name() {
        return "--not-auth";
    }

    @Override
    public String alias() {
        return "-na";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to skip the authentication request.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_SKIP_AUTHENTICATION, Boolean.TRUE.toString());
        return true;
    }

}
