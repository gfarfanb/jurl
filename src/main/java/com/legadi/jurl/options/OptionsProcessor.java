package com.legadi.jurl.options;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.legadi.jurl.exception.CommandException;

public class OptionsProcessor {

    private final Map<Option, String[]> options;
    private final Path specPath;

    public OptionsProcessor(String[] args) {
        validateArgs(args);
        this.specPath = extractSpecPath(args);
        this.options = extractOptions(args);
    }

    private void validateArgs(String[] args) {
        if(args == null || args.length < 1) {
            throw new CommandException("No args in the command, please use [--help] option");
        }
    }

    private Path extractSpecPath(String[] args) {
        return Paths.get(args[args.length - 1]);
    }

    private Map<Option, String[]> extractOptions(String[] args) {
        String[] options = new String[args.length - 1];
        System.arraycopy(args, 0, options, 0, args.length - 1);

        Map<Option, String[]> argsByOption = new HashMap<>();
        int index = 0;

        while(index < options.length) {
            try {
                Option option = Option.valueOfOpt(options[index]);
                String arguments[];

                if(option.getNumArgs() > 0) {
                    arguments = new String[option.getNumArgs()];
                    System.arraycopy(options, index + 1, arguments, 0, option.getNumArgs());
                } else {
                    arguments = new String[0];
                }

                argsByOption.put(option, arguments);
                index += arguments.length + 1;
            } catch(IndexOutOfBoundsException ex) {
                throw new CommandException("Invalid options: " + Arrays.toString(options));
            }
        }

        return argsByOption;
    }

    public Map<Option, String[]> getOptions() {
        return options;
    }

    public Path getSpecPath() {
        return specPath;
    }
}
