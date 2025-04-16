package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.JsonUtils.writeJsonFile;

import java.util.Map;

import com.legadi.cli.jurl.common.Settings;

public class EnvironmentSetValueOption extends Option {

    @Override
    public String name() {
        return "--env-set";
    }

    @Override
    public String alias() {
        return "-es";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "env", "name", "value" };
    }

    @Override
    public String getDescription() {
        return "Saves a property value to the environment file './config[.<env>].json'. It creates a new environment file if it doesn't exist.";
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
