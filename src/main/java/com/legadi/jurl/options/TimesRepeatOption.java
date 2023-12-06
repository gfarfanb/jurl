package com.legadi.jurl.options;

import static com.legadi.jurl.common.CommonUtils.isNumeric;
import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_TIMES;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class TimesRepeatOption extends Option {

    @Override
    public String name() {
        return "--times";
    }

    @Override
    public String alias() {
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
    public boolean execute(Settings settings, String[] args) {
        if(!isNumeric(args[0])) {
            throw new CommandException("Invalid number of executions: " + args[0]);
        }
        settings.putOverride(PROP_EXECUTION_TIMES, args[0]);
        return true;
    }
}
