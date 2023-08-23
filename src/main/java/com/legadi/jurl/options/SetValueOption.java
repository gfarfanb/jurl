package com.legadi.jurl.options;

import static com.legadi.jurl.common.Settings.putProperty;

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
    public int getPriority() {
        return Integer.MAX_VALUE - 2;
    }

    @Override
    public boolean execute(String[] args) {
        putProperty(args[0], args[1]);
        return true;
    }
}
