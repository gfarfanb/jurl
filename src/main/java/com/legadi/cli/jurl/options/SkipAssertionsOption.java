package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_ASSERTIONS;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--not-assert", alias = "-ns")
public class SkipAssertionsOption extends Option {

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
