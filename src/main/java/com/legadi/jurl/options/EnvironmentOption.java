package com.legadi.jurl.options;

import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.LoaderUtils.loadCredentials;

import java.nio.file.Path;

import com.legadi.jurl.common.Settings;

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
    public boolean execute(Settings settings, String[] args) {
        String environment = args[0];

        settings.setEnvironment(environment);

        Path configPath = settings.getConfigFilePath();
        Path overridePath = settings.getOverrideFilePath();
        Path credentialsPath = settings.getCredentialsFilePath();

        Settings.mergeProperties(environment, loadJsonProperties(configPath));
        Settings.mergeProperties(environment, loadJsonProperties(overridePath));
        Settings.mergeCredentials(environment, loadCredentials(credentialsPath));
        return true;
    }
}
