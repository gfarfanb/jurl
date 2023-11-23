package com.legadi.jurl.options;

import static com.legadi.jurl.executor.RequestHandlersRegistry.registerProcessor;

import com.legadi.jurl.common.Settings;

public class CustomRequestProcessorOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-request-processor";
    }

    @Override
    public String getAlias() {
        return "-crp";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "processor-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a new response processor.\nIf there is an existing processor that accepts the\nsame type of request the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerProcessor(args[0]);
        return true;
    }
}
