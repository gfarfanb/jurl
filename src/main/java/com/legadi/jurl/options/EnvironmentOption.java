package com.legadi.jurl.options;

import static com.legadi.jurl.common.Loader.loadCredentials;
import static com.legadi.jurl.common.Loader.loadJsonProperties;
import static com.legadi.jurl.common.Settings.mergeProperties;
import static com.legadi.jurl.common.Settings.mergeCredentials;

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
    public int getPriority() {
        return Integer.MAX_VALUE - 1;
    }

    @Override
    public boolean execute(String[] args) {
        mergeProperties(loadJsonProperties("./config." + args[0] + ".json", false));
        mergeCredentials(loadCredentials("./credentials." + args[0] + ".json", false));
        return true;
    }
}
