package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.register;
import static java.util.logging.Level.FINE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;

public class OptionsReader {

    private static final Logger LOGGER = Logger.getLogger(OptionsReader.class.getName());

    private final List<OptionEntry> optionEntries = new ArrayList<>();
    private final String requestInputPath;

    public OptionsReader(String[] args) {
        registerAddOnOptions();
        this.requestInputPath = extractOptionsAndRequestInputPath(args);
    }

    private void registerAddOnOptions() {
        Settings settings = new Settings();
        String[] addOnOptions = settings.getAddOnOptionClasses();

        for(String addOnOption : addOnOptions) {
            if(isBlank(addOnOption)) {
                continue;
            }

            Option option = register(Option.class, trim(addOnOption));
            LOGGER.info("Add-on registered: class=" + addOnOption
                + " opt=" + option.name()
                + " alias=" + option.alias());
        }
    }

    private String extractOptionsAndRequestInputPath(String[] args) {
        if(isEmpty(args)) {
            throw new CommandException("No args in the command, please use [--help] option");
        }

        int index = 0;
        String lastArg = null;

        while(index < args.length) {
            Optional<Option> option = null;

            try {
                option = findByName(Option.class, args[index]);

                if(!option.isPresent()) {
                    lastArg = args[index];
                    index++;
                    continue;
                }

                String[] optionArgs = option.get().getArgs();
                String arguments[];

                if(optionArgs.length > 0) {
                    arguments = new String[optionArgs.length];
                    System.arraycopy(args, index + 1, arguments, 0, optionArgs.length);
                } else {
                    arguments = new String[0];
                }

                optionEntries.add(new OptionEntry(option.get(), arguments));

                index += arguments.length + 1;
            } catch(Exception ex) {
                LOGGER.log(FINE, "Invalid option - " + ex.getMessage(), ex);
                throw new CommandException("Invalid option definition: " + Arrays.toString(args) + " - " + option);
            }
        }

        optionEntries.sort(new OptionComparator());

        if(isNotBlank(lastArg)) {
            return lastArg;
        } else {
            return null;
        }
    }

    public List<OptionEntry> getOptionEntries() {
        return optionEntries;
    }

    public String getRequestInputPath() {
        return requestInputPath;
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
