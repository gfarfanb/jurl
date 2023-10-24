package com.legadi.jurl.options;

import static com.legadi.jurl.executor.mixer.BodyMixerRegistry.registerMixer;

import com.legadi.jurl.common.Settings;

public class CustomMixerOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-mixer";
    }

    @Override
    public String getAlias() {
        return "-cm";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "mixer-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a body mixer.\nIf there is an existing mixer that accepts the\nsame body type the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerMixer(args[0]);
        return true;
    }
    
}
