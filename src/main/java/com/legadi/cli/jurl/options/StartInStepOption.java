package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_START_IN_STEP_INDEX_OR_NAME;

import com.legadi.cli.jurl.common.Settings;

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
        return new String[] { "step-index/name" };
    }

    @Override
    public String getDescription() {
        return "Tells command from which step the flow is going to start the execution. Number '1' is the first element. If a name is set, the flow is executed from the first coincidence.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putUserInput(PROP_START_IN_STEP_INDEX_OR_NAME, args[0]);
        return true;
    }
}
