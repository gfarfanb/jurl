package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_REQUEST_TYPE;

import com.legadi.jurl.common.Settings;

public class RequestTypeOption extends Option {

    @Override
    public String getOpt() {
        return "--request-type";
    }

    @Override
    public String getAlias() {
        return "-rt";
    }

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
