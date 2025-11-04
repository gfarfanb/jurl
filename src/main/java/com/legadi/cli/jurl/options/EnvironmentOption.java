package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.Settings.loadPropertiesFromGroupsFile;

import java.util.Optional;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--env", alias = "-e")
public class EnvironmentOption extends Option {

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

        Settings.mergeProperties(environment, loadJsonProperties(
            settings.getConfigFilePath()));
        Settings.mergeProperties(environment, loadPropertiesFromGroupsFile(
            Optional.of(settings), settings.getGroupsFilePath()));
        Settings.mergeProperties(environment, loadJsonProperties(
            settings.getOverrideFilePath()));

        return true;
    }
}
