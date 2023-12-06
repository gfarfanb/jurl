package com.legadi.jurl.options;

import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;

import java.nio.file.Path;

import com.legadi.jurl.common.Settings;

public class EnvironmentOption extends Option {

    @Override
    public String name() {
        return "--env";
    }

    @Override
    public String alias() {
        return "-e";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "env" };
    }

    @Override
    public String getDescription() {
        return "Loads to the settings the file: \n'./config[.<env>].json'.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        String environment = args[0];

        settings.setEnvironment(environment);

        Path configPath = settings.getConfigFilePath();
        Path overridePath = settings.getOverrideFilePath();

        Settings.mergeProperties(environment, loadJsonProperties(configPath));
        Settings.mergeProperties(environment, loadJsonProperties(overridePath));

        return true;
    }
}
