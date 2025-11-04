package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OVERRIDE_REQUEST_FILE_PATH;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--override-request", alias = "-or")
public class OverrideRequestOption extends Option {

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
