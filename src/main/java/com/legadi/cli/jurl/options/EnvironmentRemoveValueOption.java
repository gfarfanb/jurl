package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.removeJsonProperties;

import com.legadi.cli.jurl.common.Settings;

public class EnvironmentRemoveValueOption extends Option {

    @Override
    public String name() {
        return "--env-rm";
    }

    @Override
    public String alias() {
        return "-er";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "env", "name" };
    }

    @Override
    public String getDescription() {
        return "Remove a property value to the environment file './config[.<env>].json'.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        Settings envSettings = new Settings();
        envSettings.setEnvironment(args[0]);

        removeJsonProperties(envSettings.getConfigFilePath(), args[1]);

        return false;
    }
}
