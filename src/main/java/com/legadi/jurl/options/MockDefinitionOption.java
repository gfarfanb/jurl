package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_MOCK_REQUEST_CLASS;

import java.net.HttpURLConnection;

import com.legadi.jurl.common.Settings;

public class MockDefinitionOption extends Option {

    @Override
    public String getOpt() {
        return "--mock-def";
    }

    @Override
    public String getAlias() {
        return "-md";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "mock-class" };
    }

    @Override
    public String getDescription() {
        return "Tells request to use a mock class for the request connection. \nHTTP requests: <mock-class> extends " + HttpURLConnection.class.getName();
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        settings.putOverride(PROP_MOCK_REQUEST_CLASS, args[0]);
        return true;
    }
    
}
