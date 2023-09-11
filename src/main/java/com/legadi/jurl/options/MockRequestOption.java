package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_MOCK_REQUEST;

import com.legadi.jurl.common.Settings;

public class MockRequestOption extends Option {

    @Override
    public String getOpt() {
        return "--mock";
    }

    @Override
    public String getAlias() {
        return "-m";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request not to connect with the remote host but mock the request.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_MOCK_REQUEST, Boolean.TRUE.toString());
        return true;
    }
}
