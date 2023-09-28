package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_OVERRIDE_REQUEST_FILE;

import com.legadi.jurl.common.Settings;

public class OverrideRequestOption extends Option {

    @Override
    public String getOpt() {
        return "--override-request";
    }

    @Override
    public String getAlias() {
        return "-or";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "override-file" };
    }

    @Override
    public String getDescription() {
        return "Tells request to use the values defined in the file.\nThose values can be headers, params, assertions, output mappings,\ncontent, etc.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_OVERRIDE_REQUEST_FILE, args[0]);
        return true;
    }
}
