package com.legadi.jurl.options;

import com.legadi.jurl.common.Settings;
import static com.legadi.jurl.modifiers.ValueModifierRegistry.registerModifier;

public class CustomModifierOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-modifier";
    }

    @Override
    public String getAlias() {
        return "-co";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "modifier-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a value modifier.\nIf there is an existing modifier that accepts the\nsame definition the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerModifier(args[0]);
        return true;
    }
    
}
