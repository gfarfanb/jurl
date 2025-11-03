package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_EXECUTE_AUTHENTICATION;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_AUTHENTICATION;

import com.legadi.cli.jurl.common.Settings;

public class ExecuteAuthenticationOption extends Option {

    @Override
    public String name() {
        return "--exec-auth";
    }

    @Override
    public String alias() {
        return "-ea";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to execute the authentication request.";
    }

    @Override
    public boolean requiredForAuth() {
        return true;
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_EXECUTE_AUTHENTICATION, Boolean.TRUE.toString());
        settings.putOverride(PROP_SKIP_AUTHENTICATION, Boolean.FALSE.toString());
        return true;
    }
}
