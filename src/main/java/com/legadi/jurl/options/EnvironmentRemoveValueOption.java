package com.legadi.jurl.options;

import static com.legadi.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;

import java.util.Map;

import com.legadi.jurl.common.Settings;

public class EnvironmentRemoveValueOption extends Option {

    @Override
    public String getOpt() {
        return "--env-rm";
    }

    @Override
    public String getAlias() {
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

        Map<String, String> envProperties = loadJsonProperties(envSettings.getConfigFilePath());

        envProperties.remove(args[1]);

        writeJsonFile(envSettings.getConfigFilePath(), envProperties);

        return false;
    }

}
