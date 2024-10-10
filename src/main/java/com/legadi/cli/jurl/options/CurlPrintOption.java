package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.RequestBehaviour;

public class CurlPrintOption extends Option {

    @Override
    public String name() {
        return "--curl";
    }

    @Override
    public String alias() {
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
    public boolean allowedForRequestAuth() {
        return true;
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putUserInput(PROP_REQUEST_BEHAVIOUR, RequestBehaviour.CURL_ONLY.name());
        return true;
    }
}
