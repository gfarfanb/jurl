package com.legadi.jurl.options;

import static com.legadi.jurl.parser.RequestParserRegistry.registerParser;

import com.legadi.jurl.common.Settings;

public class CustomParserOption extends Option {

    @Override
    public String getOpt() {
        return "--custom-parser";
    }

    @Override
    public String getAlias() {
        return "-cp";
    }

    @Override
    public String[] getArgs() {
        return new String[] { "parser-class" };
    }

    @Override
    public String getDescription() {
        return "Registers a new request parser.\nIf there is an existing parser that accepts the\nsame type of request the last one will be taken.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        registerParser(args[0]);
        return true;
    }
}
