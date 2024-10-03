package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_USER_INPUT;

import com.legadi.cli.jurl.common.Settings;

public class SkipUserInputOption extends Option {

    @Override
    public String name() {
        return "--no-input";
    }

    @Override
    public String alias() {
        return "-ni";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to skip the user inputs.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_SKIP_USER_INPUT, Boolean.TRUE.toString());
        return true;
    }
}
