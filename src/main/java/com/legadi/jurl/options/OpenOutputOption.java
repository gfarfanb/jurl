package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_OPEN_OUTPUT_IN_EDITOR;

import com.legadi.jurl.common.Settings;

public class OpenOutputOption extends Option {

    @Override
    public String getOpt() {
        return "--open";
    }

    @Override
    public String getAlias() {
        return "-o";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to open the output in an editor.\nUses 'openEditorCommand' from request settings.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_OPEN_OUTPUT_IN_EDITOR, Boolean.TRUE.toString());
        return true;
    }
}
