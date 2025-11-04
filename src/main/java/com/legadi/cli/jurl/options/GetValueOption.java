package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.CommonUtils.ARGS_SEPARATOR;
import static com.legadi.cli.jurl.common.CommonUtils.TABS_FOR_COMPLEMENT;
import static com.legadi.cli.jurl.common.CommonUtils.splitInLinesByLength;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "--get", alias = "-g")
public class GetValueOption extends Option {

    private static final Logger LOGGER = Logger.getLogger(GetValueOption.class.getName());

    @Override
    public String[] getArgs() {
        return new String[] { "name" };
    }

    @Override
    public String getDescription() {
        return "Shows the property value.";
    }

    @Override
    public boolean execute(Settings settings, String[] args) {
        Map<String, String> values = new LinkedHashMap<>();

        values.put("Env", settings.getEnvironment());
        values.put("Property", args[0]);
        values.put("Value", settings.get(args[0]));

        printValues(settings, values);

        return false;
    }

    private void printValues(Settings settings, Map<String, String> values) {
        int maxLength = values.keySet()
            .stream()
            .mapToInt(String::length)
            .max()
            .orElse(0) + ARGS_SEPARATOR.length();
        int complementLength = maxLength
            + settings.getConsoleTabLength() * TABS_FOR_COMPLEMENT;
        StringBuilder message = new StringBuilder();

        for(Map.Entry<String, String> value : values.entrySet()) {
            String keyLine = value.getKey() + ARGS_SEPARATOR;

            message.append(settings.getTab());
            message.append(String.format("%-" + maxLength + "s", keyLine));

            List<String> lines = splitInLinesByLength(settings, value.getValue(), maxLength);
            boolean firstLine = true;

            for(String line : lines) {

                if(firstLine) {
                    message.append(settings.getTab());
                    firstLine = false;
                } else {
                    message.append(String.format("%-" + complementLength + "s", ""));
                }

                message.append(line);
                message.append("\n");
            }
        }

        LOGGER.info(message.toString());
    }
}
