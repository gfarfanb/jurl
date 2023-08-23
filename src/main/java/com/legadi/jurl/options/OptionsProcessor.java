package com.legadi.jurl.options;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

import static com.legadi.jurl.common.StringUtils.isBlank;
import static com.legadi.jurl.common.StringUtils.isNotBlank;
import static com.legadi.jurl.options.OptionsRegistry.getOptionByArg;
import static com.legadi.jurl.options.OptionsRegistry.registerAddOn;
import static com.legadi.jurl.options.OptionsRegistry.registerOption;

public class OptionsProcessor {

    private static final Logger LOGGER = Logger.getLogger(OptionsProcessor.class.getName());

    private final List<OptionEntry> optionEntries = new LinkedList<>();
    private final Path specPath;

    public OptionsProcessor(String[] args) {
        validateArgs(args);
        registerOptions();

        this.specPath = extractOptionsAndSpecPath(args);
    }

    private void validateArgs(String[] args) {
        if(args == null || args.length < 1) {
            throw new CommandException("No args in the command, please use [--help] option");
        }
    }

    private void registerOptions() {
        registerOption(new AuthorizationBasicOption());
        registerOption(new AuthorizationTokenOption());
        registerOption(new CurlPrintOption());
        registerOption(new EnvironmentOption());
        registerOption(new HelpOption());
        registerOption(new MockDefinitionOption());
        registerOption(new MockRequestOption());
        registerOption(new OpenOutputOption());
        registerOption(new SetValueOption());
        registerOption(new TimesRepeatOption());
    }

    private Path extractOptionsAndSpecPath(String[] args) {
        int index = 0;
        String lastArg = null;

        while(index < args.length) {
            Option option = null;

            try {
                option = getOptionByArg(args[index]);

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
                    throw new CommandException("Invalid option definition: " + option);
                } else {
                    throw new CommandException("Invalid options: " + Arrays.toString(args));
                }
            }
        }

        optionEntries.sort(new OptionComparator());

        if(isNotBlank(lastArg)) {
            return Paths.get(lastArg);
        } else {
            return null;
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

    public Path getSpecPath() {
        return specPath;
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
