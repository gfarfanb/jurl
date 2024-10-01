package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.CommonUtils.isNumeric;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_START_IN_STEP_INDEX;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;

public class StartInStepOption extends Option {

    @Override
    public String name() {
        return "--start-in";
    }

    @Override
    public String alias() {
        return "-si";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "step-index" };
    }

    @Override
    public String getDescription() {
        return "Tells command from which step the flow is going to start the execution.\nNumber '1' is the first element.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        if(!isNumeric(args[0])) {
            throw new CommandException("Invalid start index: " + args[0]);
        }
        settings.putOverride(PROP_START_IN_STEP_INDEX, args[0]);
        return true;
    }
}
