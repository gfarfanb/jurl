package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.Settings.loadPropertiesFromGroupsFile;

import java.nio.file.Path;

import com.legadi.cli.jurl.common.Settings;

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
        return "Loads to the settings the file: './config[.<env>].json'";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        String environment = args[0];

        settings.setEnvironment(environment);

        Path overridePath = settings.getOverrideFilePath();
        Path groupsPath = settings.getGroupsFilePath();
        Path configPath = settings.getConfigFilePath();

        Settings.mergeProperties(environment, loadJsonProperties(overridePath));
        Settings.mergeProperties(environment, loadPropertiesFromGroupsFile(groupsPath));
        Settings.mergeProperties(environment, loadJsonProperties(configPath));

        return true;
    }
}
