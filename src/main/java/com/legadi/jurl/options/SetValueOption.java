package com.legadi.jurl.options;

import com.legadi.jurl.common.SettingsSetter;

public class SetValueOption extends Option {

    @Override
    public String getOpt() {
        return "--set";
    }

    @Override
    public String getAlias() {
        return "-s";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "name", "value" };
    }

    @Override
    public String getDescription() {
        return "Overrides a property value from request settings.";
    }

    @Override
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.putPriority(getOpt(), args[0], args[1]);
        return true;
    }
}
