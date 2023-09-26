package com.legadi.jurl.options;

import com.legadi.jurl.common.Settings;
import static com.legadi.jurl.common.generators.GeneratorsRegistry.registerGenerator;

public class CustomGeneratorOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-generator";
    }

    @Override
    public String getAlias() {
        return "-cg";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "generator-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a new generator that will be used during\nthe string replacing. If there is an existing\ngenerator that defines the same function-param\nthe last one will be taken. E.g.:\n{{CUSTOM_GENERATOR_FUNCTION}}";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerGenerator(args[0]);
        return true;
    }
}
