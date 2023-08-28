package com.legadi.jurl.options;

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
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.putEnvironment(args[0]);
        return true;
    }
}
