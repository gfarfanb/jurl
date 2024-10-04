package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredByClassOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredByNameOf;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.Settings;

public class HelpOption extends Option {

    private static final Logger LOGGER = Logger.getLogger(HelpOption.class.getName());

    private static final String COLON = ":";
    private static final int TABS_FOR_COMPLEMENT = 3;

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

        String os = System.getProperty("jurl.os.name").toLowerCase(Locale.ROOT);
        if(os.contains("win")) {
            helpMessage.append("jurl.bat ");
        } else {
            helpMessage.append("sh jurl.sh ");
        }

        helpMessage.append("[<option>]* [--help] [<add-on-option>]* <request-file>\n\n");
        helpMessage.append("Options:\n");
        appendOptions(settings, getAllRegisteredByClassOf(Option.class), helpMessage);
        helpMessage.append("\n");
        helpMessage.append("Add-on options:\n");
        appendOptions(settings, getAllRegisteredByNameOf(Option.class), helpMessage);

        LOGGER.info(helpMessage.toString());

        return false;
    }

    private void appendOptions(Settings settings, List<Option> options, StringBuilder helpMessage) {
        List<Option> opts = new ArrayList<>(options);
        int maxLength = options.stream()
            .map(Option::toString)
            .mapToInt(String::length)
            .max()
            .orElse(0) + COLON.length();
        int complementLength = maxLength
            + settings.getConsoleTabLength() * TABS_FOR_COMPLEMENT;

        opts.sort((o1, o2) -> o1.name().compareTo(o2.name()));

        for(Option option : opts) {
            String optionLine = option.toString() + COLON;

            helpMessage.append(String.format("%-" + settings.getConsoleTabLength() + "s", ""));
            helpMessage.append(String.format("%-" + maxLength + "s", optionLine));

            boolean firstLine = true;

            for(String line : option.getDescription().split("\\r?\\n")) {

                if(firstLine) {
                    helpMessage.append(String.format("%-" + settings.getConsoleTabLength() + "s", ""));
                    firstLine = false;
                } else {
                    helpMessage.append(String.format("%-" + complementLength + "s", ""));
                }

                helpMessage.append(line);
                helpMessage.append("\n");
            }
        }
    }
}
