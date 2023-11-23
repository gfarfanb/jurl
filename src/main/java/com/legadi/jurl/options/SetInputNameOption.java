package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_INPUT_NAME;

import com.legadi.jurl.common.Settings;

public class SetInputNameOption extends Option {

    @Override
    public String getOpt() {
        return "--name";
    }

    @Override
    public String getAlias() {
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
        settings.putUserInput(PROP_INPUT_NAME, inputName);
    }
}
