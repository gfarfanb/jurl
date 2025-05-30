package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_CONDITIONS;

import com.legadi.cli.jurl.common.Settings;

public class SkipConditionsOption extends Option {

    @Override
    public String name() {
        return "--no-conditions";
    }

    @Override
    public String alias() {
        return "-nc";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to skip the conditions evaluation.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_SKIP_CONDITIONS, Boolean.TRUE.toString());
        return true;
    }
}
