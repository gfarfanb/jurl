package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_TYPE;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--request-type", alias = "-rt")
public class RequestTypeOption extends Option {

    @Override
    public String[] getArgs() {
        return new String[] { "request-type" };
    }

    @Override
    public String getDescription() {
        return "Tells the type of the request to process the input file.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_REQUEST_TYPE, args[0]);
        return true;
    }
}
