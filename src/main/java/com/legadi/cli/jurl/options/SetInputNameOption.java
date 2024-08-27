package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_INPUT_NAME;

import com.legadi.cli.jurl.common.Settings;

public class SetInputNameOption extends Option {

    @Override
    public String name() {
        return "--name";
    }

    @Override
    public String alias() {
        return "-n";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "input-name" };
    }

    @Override
    public String getDescription() {
        return "Tells command which request/flow from request file must pick.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        setInputName(settings, args[0]);
        return true;
    }

    public static void setInputName(Settings settings, String inputName) {
        settings.putOverride(PROP_INPUT_NAME, inputName);
    }
}
