package com.legadi.cli.jurl.options;

import com.legadi.cli.jurl.common.Settings;

public class SetValueOption extends Option {

    @Override
    public String name() {
        return "--set";
    }

    @Override
    public String alias() {
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
    public boolean requiredForAuth() {
        return true;
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putUserInput(args[0], args[1]);
        return true;
    }
}
