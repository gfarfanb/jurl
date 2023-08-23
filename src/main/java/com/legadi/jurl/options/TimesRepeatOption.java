package com.legadi.jurl.options;

import static com.legadi.jurl.common.Settings.putProperty;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_TIMES;

public class TimesRepeatOption extends Option {

    @Override
    public String getOpt() {
        return "--times";
    }

    @Override
    public String getAlias() {
        return "-t";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "value" };
    }

    @Override
    public String getDescription() {
        return "Sets the number of executions required for the request.";
    }

    @Override
    public boolean execute(String[] args) {
        putProperty(PROP_EXECUTION_TIMES, args[0]);
        return true;
    }
}
