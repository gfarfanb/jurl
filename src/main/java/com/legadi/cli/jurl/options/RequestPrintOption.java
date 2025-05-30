package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.RequestBehaviour;

public class RequestPrintOption extends Option {

    @Override
    public String name() {
        return "--print";
    }

    @Override
    public String alias() {
        return "-p";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Displays the request definition.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putUserInput(PROP_REQUEST_BEHAVIOUR, RequestBehaviour.PRINT_ONLY.name());
        return true;
    }
}
