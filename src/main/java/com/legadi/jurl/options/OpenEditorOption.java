package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_OPEN_OUTPUT_IN_EDITOR;

import com.legadi.jurl.common.Settings;

public class OpenEditorOption extends Option {

    @Override
    public String name() {
        return "--open-editor";
    }

    @Override
    public String alias() {
        return "-oe";
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
