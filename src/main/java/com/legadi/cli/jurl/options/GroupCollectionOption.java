package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.CommonUtils.toGroupParam;

import java.util.Optional;

import com.legadi.cli.jurl.common.Settings;

public class GroupCollectionOption extends Option {

    @Override
    public String name() {
        return "--group-coll";
    }

    @Override
    public String alias() {
        return "-gc";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "group-name", "collection-tag" };
    }

    @Override
    public String getDescription() {
        return "Overrides the active field value from './groups[.<env>].json' for a specific group.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        String groupParam = toGroupParam(Optional.of(settings), args[0]);
        settings.putUserInput(groupParam, args[1]);
        return true;
    }
}
