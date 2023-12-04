package com.legadi.jurl.options;

import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;

import java.util.Map;

import com.legadi.jurl.common.Settings;

public class EnvironmentSetValueOption extends Option {

    @Override
    public String getOpt() {
        return "--set-env";
    }

    @Override
    public String getAlias() {
        return "-se";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "env", "name", "value" };
    }

    @Override
    public String getDescription() {
        return "Saves a property value to the environment file './config[.<env>].json'.\nIt creates a new environment file if it doesn't exist.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        Settings envSettings = new Settings();
        envSettings.setEnvironment(args[0]);

        Map<String, String> envProperties = loadJsonProperties(envSettings.getConfigFilePath());

        envProperties.put(args[1], args[2]);

        writeJsonFile(envSettings.getConfigFilePath(), envProperties);

        return false;
    }

}
