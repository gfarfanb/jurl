package com.legadi.jurl.options;

import static com.legadi.jurl.common.Loader.loadCredentials;
import static com.legadi.jurl.common.Loader.loadJsonProperties;
import static com.legadi.jurl.common.SettingsConstants.PROP_ENVIRONMENT;

import com.legadi.jurl.common.SettingsSetter;

public class EnvironmentOption extends Option {

    @Override
    public String getOpt() {
        return "--env";
    }

    @Override
    public String getAlias() {
        return "-e";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "env" };
    }

    @Override
    public String getDescription() {
        return "Loads to the settings the following files:\n'./config[.<env>].json' and (optional) './credentials[.<env>].json'.";
    }

    @Override
    public int getPriority() {
        return Integer.MAX_VALUE - 1;
    }

    @Override
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.put(PROP_ENVIRONMENT, args[0]);
        settings.mergeProperties(loadJsonProperties("./config." + args[0] + ".json", false));
        settings.mergeCredentials(loadCredentials("./credentials." + args[0] + ".json", false));
        return true;
    }
}
