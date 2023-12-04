package com.legadi.jurl.options;

import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;

import java.util.Map;

import com.legadi.jurl.common.Settings;

public class EnvironmentCopyOption extends Option {

    @Override
    public String getOpt() {
        return "--env-copy";
    }

    @Override
    public String getAlias() {
        return "-ec";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "env-from", "env-to" };
    }

    @Override
    public String getDescription() {
        return "Copies the content of a environment file './config[.<env>].json' to another one.\nIt creates a new environment file if it doesn't exist.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        Settings fromEnv = new Settings();
        fromEnv.setEnvironment(args[0]);

        Settings toEnv = new Settings();
        toEnv.setEnvironment(args[1]);

        Map<String, String> fromProperties = loadJsonProperties(fromEnv.getConfigFilePath());
        Map<String, String> toProperties = loadJsonProperties(toEnv.getConfigFilePath());

        toProperties.putAll(fromProperties);

        writeJsonFile(toEnv.getConfigFilePath(), toProperties);

        return false;
    }

}
