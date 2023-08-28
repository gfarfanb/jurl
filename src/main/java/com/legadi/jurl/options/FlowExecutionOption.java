package com.legadi.jurl.options;

import static com.legadi.jurl.common.SettingsConstants.PROP_EXECUTION_AS_FLOW;

import com.legadi.jurl.common.SettingsSetter;

public class FlowExecutionOption extends Option {

    @Override
    public String getOpt() {
        return "--flow";
    }

    @Override
    public String getAlias() {
        return "-f";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Tells request to execute as flow. The request file can defines\na request or a sequential execution of several requests (flow).";
    }

    @Override
    public boolean execute(SettingsSetter settings, String[] args) {
        settings.put(getOpt(), PROP_EXECUTION_AS_FLOW, Boolean.TRUE.toString());
        return true;
    }
    
}
