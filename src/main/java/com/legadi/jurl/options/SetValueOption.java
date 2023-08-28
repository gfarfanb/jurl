package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_ENVIRONMENT;

import java.util.logging.Logger;

import com.legadi.jurl.common.SettingsSetter;

public class SetValueOption extends Option {

    private static final Logger LOGGER = Logger.getLogger(SetValueOption.class.getName());

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
        if(PROP_ENVIRONMENT.equals(args[0])) {
            LOGGER.warning("Property [" + args[0] + "] can't be modified use [--env] option instead");
            return true;
        }

        settings.putPriority(args[0], args[1]);
        return true;
    }
}
