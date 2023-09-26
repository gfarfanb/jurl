package com.legadi.jurl.options;

import static com.legadi.jurl.executor.RequestHandlersRegistry.registerHandler;

import com.legadi.jurl.common.Settings;

public class CustomHandlerOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-handler";
    }

    @Override
    public String getAlias() {
        return "-ch";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "executor-class", "processor-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a new request executor and response processor.\nIf there is an existing executor that accepts the\nsame type of request the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerHandler(args[0], args[1]);
        return true;
    }
}
