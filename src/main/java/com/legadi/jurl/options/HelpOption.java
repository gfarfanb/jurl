package com.legadi.jurl.options;

import static com.legadi.jurl.common.ObjectsRegistry.getAllRegisteredByClassOf;
import static com.legadi.jurl.common.ObjectsRegistry.getAllRegisteredByNameOf;

import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import com.legadi.jurl.common.Settings;

public class HelpOption extends Option {

    private static final Logger LOGGER = Logger.getLogger(HelpOption.class.getName());

    private static final int TAB_LENGTH = 2;

    @Override
    public String name() {
        return "--help";
    }

    @Override
    public String alias() {
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
    public boolean execute(Settings settings, String[] args) {
        StringBuilder helpMessage = new StringBuilder();
        helpMessage.append("CLI Java application to provides an API test/client tool.\n\n");
        helpMessage.append("Usage: ");

        String os = System.getProperty("os.name").toLowerCase(Locale.ROOT);
        if(os.contains("win")) {
            helpMessage.append("jurl.bat ");
        } else {
            helpMessage.append("sh jurl.sh ");
        }

        helpMessage.append("[<option>]* [--help] [<add-on-option>]* <request-file>\n\n");
        helpMessage.append("Options:\n");
        appendOptions(getAllRegisteredByClassOf(Option.class), helpMessage);
        helpMessage.append("\n");
        helpMessage.append("Add-on options:\n");
        appendOptions(getAllRegisteredByNameOf(Option.class), helpMessage);

        LOGGER.info(helpMessage.toString());

        return false;
    }

    private void appendOptions(List<Option> options, StringBuilder helpMessage) {
        List<Option> opts = new LinkedList<>(options);
        int maxLength = options.stream()
            .map(Option::toString)
            .mapToInt(String::length)
            .max()
            .orElse(0) + TAB_LENGTH + 1;

        opts.sort((o1, o2) -> o1.name().compareTo(o2.name()));

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
