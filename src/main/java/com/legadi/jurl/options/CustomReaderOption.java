package com.legadi.jurl.options;

import static com.legadi.jurl.executor.reader.OutputReaderRegistry.registerReader;

import com.legadi.jurl.common.Settings;

public class CustomReaderOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-reader";
    }

    @Override
    public String getAlias() {
        return "-cr";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "reader-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a response reader.\nIf there is an existing reader that accepts the\nsame MIME types the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerReader(args[0]);
        return true;
    }
}
