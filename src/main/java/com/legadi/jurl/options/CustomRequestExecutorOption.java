package com.legadi.jurl.options;

import static com.legadi.jurl.executor.RequestHandlersRegistry.registerExecutor;

import com.legadi.jurl.common.Settings;

public class CustomRequestExecutorOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-request-executor";
    }

    @Override
    public String getAlias() {
        return "-cre";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "executor-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a new request executor.\nIf there is an existing executor that accepts the\nsame type of request the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerExecutor(args[0]);
        return true;
    }
}
