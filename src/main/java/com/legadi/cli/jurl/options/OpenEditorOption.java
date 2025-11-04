package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OPEN_OUTPUT_IN_EDITOR;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--open-editor", alias = "-oe")
public class OpenEditorOption extends Option {

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to open the output in an editor. Uses 'openEditorCommand' from request settings.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_OPEN_OUTPUT_IN_EDITOR, Boolean.TRUE.toString());
        return true;
    }
}
