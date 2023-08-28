package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_CURL_REQUEST;

import com.legadi.jurl.common.SettingsSetter;

public class CurlPrintOption extends Option {

    @Override
    public String getOpt() {
        return "--curl";
    }

    @Override
    public String getAlias() {
        return "-c";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Displays the related 'curl' command.";
    }

    @Override
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.put(getOpt(), PROP_CURL_REQUEST, Boolean.TRUE.toString());
        return true;
    }
}
