package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;

import com.legadi.cli.jurl.common.Settings;

public class SkipAssertionsOption extends Option {

    @Override
    public String name() {
        return "--not-assert";
    }

    @Override
    public String alias() {
        return "-ns";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to skip the assertions validation.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_SKIP_ASSERTIONS, Boolean.TRUE.toString());
        return true;
    }

}
