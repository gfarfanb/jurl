package com.legadi.jurl.options;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.options.OptionsRegistry.findByArg;
import static com.legadi.jurl.options.OptionsRegistry.registerAddOn;

public class OptionsReader {

    private static final Logger LOGGER = Logger.getLogger(OptionsReader.class.getName());

    private final List<OptionEntry> optionEntries = new ArrayList<>();
    private final String requestInputPath;

    public OptionsReader(String[] args) {
        this.requestInputPath = extractOptionsAndRequestInputPath(args);
    }

    private String extractOptionsAndRequestInputPath(String[] args) {
        validateArgs(args);

        int index = 0;
        String lastArg = null;

        while(index < args.length) {
            Option option = null;

            try {
                option = findByArg(args[index]);

                if(option == null) {
                    lastArg = args[index];
                    index++;
                    continue;
                }

                String[] optionArgs = option.getArgs();
                String arguments[];

                if(optionArgs.length > 0) {
                    arguments = new String[optionArgs.length];
                    System.arraycopy(args, index + 1, arguments, 0, optionArgs.length);
                } else {
                    arguments = new String[0];
                }

                optionEntries.add(new OptionEntry(option, arguments));

                index += arguments.length + 1;
            } catch(Exception ex) {
                if(option != null) {
                    throw new CommandException("Invalid option definition: " + Arrays.toString(args) + " - " + option);
                } else {
                    throw new CommandException("Invalid options: " + Arrays.toString(args));
                }
            }
        }

        optionEntries.sort(new OptionComparator());

        if(isNotBlank(lastArg)) {
            return lastArg;
        } else {
            return null;
        }
    }

    private void validateArgs(String[] args) {
        if(args == null || args.length < 1) {
            throw new CommandException("No args in the command, please use [--help] option");
        }
    }

    @SuppressWarnings("unchecked")
    public void registerAddOnOptions() {
        Settings settings = new Settings();
        String[] addOnOptions = settings.getAddOnOptionClasses();

        if(addOnOptions.length > 0) {

            for(String addOnOption : addOnOptions) {

                if(isBlank(addOnOption)) {
                    continue;
                }

                try {
                    Option option = registerAddOn((Class<Option>) Class.forName(addOnOption.trim()));
                    LOGGER.info("Add-on registered: class=" + addOnOption
                        + " opt=" + option.getOpt()
                        + " alias=" + option.getAlias());
                } catch(ClassCastException | ClassNotFoundException ex) {
                    throw new CommandException("Unable to load add-on: " + addOnOption);
                }
            }
        }
    }

    public List<OptionEntry> getOptionEntries() {
        return optionEntries;
    }

    public String getRequestInputPath() {
        return requestInputPath;
    }

    public List<OptionEntry> mapToOptionEntries(Map<String, String[]> options) {
        if(options == null) {
            return new LinkedList<>();
        }

        return options.entrySet()
            .stream()
            .map(e -> mapToOptionEntry(e.getKey(), e.getValue()))
            .sorted(new OptionComparator())
            .collect(Collectors.toList());
    }

    public OptionEntry mapToOptionEntry(String opt, String[] arguments) {
        Option option = findByArg(opt);

        if(option == null) {
            throw new CommandException("Option not found: " + opt);
        }
        if(option.getArgs().length > 0
                && (arguments == null || arguments.length < option.getArgs().length)) {
            throw new CommandException("Invalid args length for option: "
                + Arrays.toString(arguments) + " - " + option);
        }

        return new OptionEntry(option, arguments);
    }

    public static class OptionEntry extends Pair<Option, String[]> {

        public OptionEntry(Option option, String[] arguments) {
            super(option, arguments);
        }
    }

    public static class OptionComparator implements Comparator<OptionEntry> {

        @Override
        public int compare(OptionEntry o1, OptionEntry o2) {
            return o1.getLeft().getPriority() - o2.getLeft().getPriority();
        }
        
    }
}
