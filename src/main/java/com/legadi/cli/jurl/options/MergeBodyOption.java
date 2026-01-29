package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--merge-body", alias = "-mb")
public class MergeBodyOption extends Option {

    @Override
    public String[] getArgs() {
        return new String[] { "body-type" };
    }

    @Override
    public String getDescription() {
        return "Tells request to combine the request body ('bodyContent' or 'bodyFilePath') with 'bodyMergePath'. Normal behaviour is: request body as base and body merge as compare. Behaviour can be changed by using 'bodyMergeAsBase' as 'true'.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_MERGE_BODY_USING_TYPE, args[0]);
        return true;
    }
}
