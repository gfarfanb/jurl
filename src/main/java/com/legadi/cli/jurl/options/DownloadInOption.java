package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SAVE_OUTPUT_IN_LOCATION;

import com.legadi.cli.jurl.common.Settings;

public class DownloadInOption extends Option {

    @Override
    public String name() {
        return "--download-in";
    }

    @Override
    public String alias() {
        return "-di";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to save the output in a download directory.\nUses 'downloadsLocation' from request settings.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_SAVE_OUTPUT_IN_LOCATION, Boolean.TRUE.toString());
        return true;
    }
}
