package com.legadi.jurl.options;

import static com.legadi.jurl.options.OptionsRegistry.getAddOns;
import static com.legadi.jurl.options.OptionsRegistry.getOptions;

import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Logger;

import com.legadi.jurl.common.SettingsSetter;

public class HelpOption extends Option {

    private static final Logger LOGGER = Logger.getLogger("");

    private static final int TAB_LENGTH = 2;

    @Override
    public String getOpt() {
        return "--help";
    }

    @Override
    public String getAlias() {
        return "-h";
    }

    @Override
    public String[] getArgs() {
        return new String[0];
    }

    @Override
    public String getDescription() {
        return "Displays this help message.";
    }

    @Override
    public int getPriority() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean execute(SettingsSetter settings, String[] args) {
        StringBuilder helpMessage = new StringBuilder();
        helpMessage.append("Simple API development environment by command line and Java.\n\n");
        helpMessage.append("Usage: ");

        String os = System.getProperty("os.name").toLowerCase(Locale.ROOT);
        if(os.contains("win")) {
            helpMessage.append("jurl.bat ");
        } else {
            helpMessage.append("sh jurl.sh ");
        }

        helpMessage.append("[<option>]* [--help] [<add-on-option>]* <request-file>[.json]\n\n");
        helpMessage.append("Options:\n");
        appendOptions(getOptions(), helpMessage);
        helpMessage.append("\n");
        helpMessage.append("Add-on options:\n");
        appendOptions(getAddOns(), helpMessage);

        LOGGER.info(helpMessage.toString());

        return false;
    }

    private void appendOptions(Set<Option> options, StringBuilder helpMessage) {
        List<Option> opts = new LinkedList<>(options);
        int maxLength = options.stream()
            .map(Option::toString)
            .mapToInt(String::length)
            .max()
            .orElse(0) + TAB_LENGTH + 1;

        opts.sort((o1, o2) -> o1.getOpt().compareTo(o2.getOpt()));

        for(Option option : opts) {
            String optionLine = option.toString();

            helpMessage.append(String.format("%-" + TAB_LENGTH + "s", ""));
            helpMessage.append(String.format("%-" + maxLength + "s", optionLine + ":"));

            boolean firstLine = true;

            for(String line : option.getDescription().split("\\r?\\n")) {

                if(firstLine) {
                    helpMessage.append(String.format("%-" + TAB_LENGTH + "s", ""));
                    firstLine = false;
                } else {
                    helpMessage.append(String.format("%-" + (TAB_LENGTH + maxLength + TAB_LENGTH + TAB_LENGTH) + "s", ""));
                }

                helpMessage.append(line);
                helpMessage.append("\n");
            }
        }
    }
}
