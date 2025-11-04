package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractConfigReplaceableProperties;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;
import static com.legadi.cli.jurl.common.CommonUtils.ARGS_SEPARATOR;
import static com.legadi.cli.jurl.common.CommonUtils.TABS_FOR_COMPLEMENT;
import static com.legadi.cli.jurl.common.CommonUtils.splitInLinesByLength;
import static com.legadi.cli.jurl.common.JsonUtils.loadInternalJsonProperties;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredByClassOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredByNameOf;
import static com.legadi.cli.jurl.common.ObjectsRegistry.getAllRegisteredConfigReplaceable;
import static com.legadi.cli.jurl.common.SettingsConstants.DEFAULT_SETTINGS_FILE;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--help", alias = "-h")
public class HelpOption extends Option {

    private static final Logger LOGGER = Logger.getLogger(HelpOption.class.getName());

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

        if(settings.isWindowsOS()) {
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
        helpMessage.append("\n");
        helpMessage.append("Setting properties (replaceable in './config[.<env>].json'):\n");
        appendConfig(settings, helpMessage);
        helpMessage.append("\n");
        helpMessage.append("Extension properties (replaceable in './config[.<env>].json'):\n");
        appendConfig(settings, getAllRegisteredConfigReplaceable(), helpMessage);

        LOGGER.info(helpMessage.toString());

        return false;
    }

    private void appendOptions(Settings settings, List<Option> options, StringBuilder helpMessage) {
        List<Option> opts = new ArrayList<>(options);
        int maxLength = options.stream()
            .map(Option::toString)
            .mapToInt(String::length)
            .max()
            .orElse(0) + ARGS_SEPARATOR.length();
        int complementLength = maxLength
            + settings.getConsoleTabLength() * TABS_FOR_COMPLEMENT;

        opts.sort((o1, o2) -> extractNamedName(o1).compareTo(extractNamedName(o2)));

        for(Option option : opts) {
            String optionLine = option.toString() + ARGS_SEPARATOR;

            helpMessage.append(settings.getTab());
            helpMessage.append(String.format("%-" + maxLength + "s", optionLine));

            List<String> lines = splitInLinesByLength(settings, option.getDescription(), maxLength);
            boolean firstLine = true;

            for(String line : lines) {

                if(firstLine) {
                    helpMessage.append(settings.getTab());
                    firstLine = false;
                } else {
                    helpMessage.append(String.format("%-" + complementLength + "s", ""));
                }

                helpMessage.append(line);
                helpMessage.append("\n");
            }
        }
    }

    private void appendConfig(Settings settings, StringBuilder helpMessage) {
        Map<String, String> defaultSettings = loadInternalJsonProperties(DEFAULT_SETTINGS_FILE);

        String properties = String.join(", ", defaultSettings.keySet());
        List<String> lines = splitInLinesByLength(settings, properties, TABS_FOR_COMPLEMENT);

        for(String line : lines) {
            helpMessage.append(settings.getTab());
            helpMessage.append(line);
            helpMessage.append("\n");
        }
    }

    private void appendConfig(Settings settings, List<Object> configs, StringBuilder helpMessage) {
        int complementLength = settings.getConsoleTabLength() * TABS_FOR_COMPLEMENT;

        for(Object config : configs) {
            helpMessage.append(settings.getTab());
            helpMessage.append(config.getClass().getName() + ARGS_SEPARATOR);
            helpMessage.append("\n");

            String properties = String.join(", ", extractConfigReplaceableProperties(config));
            List<String> lines = splitInLinesByLength(settings, properties, TABS_FOR_COMPLEMENT);

            for(String line : lines) {
                helpMessage.append(String.format("%-" + complementLength + "s", ""));
                helpMessage.append(line);
                helpMessage.append("\n");
            }
        }
    }
}
