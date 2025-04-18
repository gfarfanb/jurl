package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;

import com.legadi.cli.jurl.common.Settings;

public class MergeBodyOption extends Option {

    @Override
    public String name() {
        return "--merge-body";
    }

    @Override
    public String alias() {
        return "-mb";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "body-type" };
    }

    @Override
    public String getDescription() {
        return "Tells request to merge body file with body content using the specified type.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_MERGE_BODY_USING_TYPE, args[0]);
        return true;
    }
}
