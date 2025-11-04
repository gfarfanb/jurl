package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_FILTER_NAME;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--filter", alias = "-f")
public class SetFilterNameOption extends Option {

    @Override
    public String[] getArgs() {
        return new String[] { "filter-name" };
    }

    @Override
    public String getDescription() {
        return "Tells command to filter requests/flows from request file.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_FILTER_NAME, args[0]);
        return true;
    }
}
