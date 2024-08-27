package com.legadi.cli.jurl.model;

import java.util.List;

import com.legadi.cli.jurl.options.OptionsReader.OptionEntry;

public class StepEntry {

    private String requestInputPath;
    private List<OptionEntry> options;

    public String getRequestInputPath() {
        return requestInputPath;
    }

    public void setRequestInputPath(String requestInputPath) {
        this.requestInputPath = requestInputPath;
    }

    public List<OptionEntry> getOptions() {
        return options;
    }

    public void setOptions(List<OptionEntry> options) {
        this.options = options;
    }
}
