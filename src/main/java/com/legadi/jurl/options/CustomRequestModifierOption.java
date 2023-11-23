package com.legadi.jurl.options;

import static com.legadi.jurl.executor.RequestHandlersRegistry.registerModifier;

import com.legadi.jurl.common.Settings;

public class CustomRequestModifierOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-request-modifier";
    }

    @Override
    public String getAlias() {
        return "-crm";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "modifier-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a new request modifier.\nIf there is an existing modifier that accepts the\nsame type of request the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerModifier(args[0]);
        return true;
    }
}
