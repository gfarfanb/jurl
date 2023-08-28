package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_INPUT_NAME;

import com.legadi.jurl.common.SettingsSetter;

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
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.put(getOpt(), PROP_INPUT_NAME, args[0]);
        return true;
    }
    
}
