package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OVERRIDE_REQUEST_FILE_PATH;

import com.legadi.cli.jurl.common.Settings;

public class OverrideRequestOption extends Option {

    @Override
    public String name() {
        return "--override-request";
    }

    @Override
    public String alias() {
        return "-or";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "override-file" };
    }

    @Override
    public String getDescription() {
        return "Tells request to use the values defined in the file. Those values can be headers, params, assertions, output mappings, content, etc.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_OVERRIDE_REQUEST_FILE_PATH, args[0]);
        return true;
    }
}
