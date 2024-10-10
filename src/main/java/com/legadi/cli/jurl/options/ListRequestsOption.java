package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.CommonUtils.formatAsOrderedList;
import static com.legadi.cli.jurl.common.CommonUtils.formatInColumns;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;

import java.io.File;
import java.util.List;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.InputNameResolver;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.RequestInput;
import com.legadi.cli.jurl.parser.RequestParser;

public class ListRequestsOption extends Option {

    private static final Logger LOGGER = Logger.getLogger(ListRequestsOption.class.getName());

    @Override
    public String name() {
        return "--list-all";
    }

    @Override
    public String alias() {
        return "-la";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Scans all the files in the current directory and shows the requests names in those files.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        RequestParser<?> requestParser = findOrFail(RequestParser.class, settings.getRequestType());
        File currentDirectory = settings.getWorkspacePath().toFile();

        for (File file : currentDirectory.listFiles()) {
            if (file.isFile()) {
                readFile(settings, requestParser, file);
            }
        }

        return false;
    }

    private void readFile(Settings settings, RequestParser<?> requestParser, File file) {
        try {
            RequestInput<?> requestInput = requestParser.parseInput(settings, file.toPath());
            InputNameResolver inputNameResolver = new InputNameResolver(settings, file.toString(), requestInput);
            List<String> requestNames = formatAsOrderedList(inputNameResolver.listRequestNames(),
                (i, name) -> name.equalsIgnoreCase(requestInput.getDefaultRequest()),
                name -> inputNameResolver.appendType(name));
            StringBuilder output = formatInColumns(settings, requestNames);

            LOGGER.info("Request-Input-Path: " + file);
            LOGGER.info(output.toString());
        } catch(Exception ex) {
            LOGGER.fine("Invalid request input file: " + file);
        }
    }
}
