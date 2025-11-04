package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_REQUEST_BEHAVIOUR;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.model.RequestBehaviour;

@Named(name = "--print", alias = "-p")
public class RequestPrintOption extends Option {

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
