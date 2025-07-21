package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.INVALID_INDEX;
import static com.legadi.cli.jurl.common.CommonUtils.START_INDEX;
import static com.legadi.cli.jurl.common.CommonUtils.formatAsOrderedList;
import static com.legadi.cli.jurl.common.CommonUtils.formatInColumns;
import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.trim;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.legadi.cli.jurl.exception.ConsoleInputException;

public class ConsoleInput {

    public static final String INPUT_FORMAT = "Enter input: '%s'> ";
    public static final String INPUT_DEFAULT_FORMAT = "Enter input: '%s' [default: '%s']> ";
    public static final String PASSWORD_FORMAT = "Enter pswrd: '%s'> ";
    public static final String OPTION_INPUT = "Select an option> ";
    public static final String OPTION_INPUT_NAME = "Select an option for input: '%s'> ";

    private final Settings settings;
    private final Function<String, String> optionDecorator;

    public ConsoleInput(Settings settings) {
        this(settings, null);
    }

    public ConsoleInput(Settings settings, Function<String, String> optionDecorator) {
        this.settings = settings;
        this.optionDecorator = optionDecorator != null
            ? optionDecorator : opt -> opt;
    }

    public String readInput(String message, String defaultValue) {
        if(settings.isSkipUserInput()) {
            return defaultValue;
        }

        String inputMessage = isNotBlank(defaultValue)
            ? String.format(INPUT_DEFAULT_FORMAT, message, defaultValue)
            : String.format(INPUT_FORMAT, message);

        return readLine(inputMessage)
            .filter(CommonUtils::isNotBlank)
            .orElse(defaultValue);
    }

    public String readPassword(String message, String defaultValue) {
        if(settings.isSkipUserInput()) {
            return defaultValue;
        }

        String inputMessage = String.format(PASSWORD_FORMAT, message);

        return readPassword(inputMessage)
            .filter(Objects::nonNull)
            .map(String::valueOf)
            .orElse(defaultValue);
    }

    public Optional<String> selectOption(List<String> options, String defaultOption,
            String namedInput) {
        if(settings.isSkipUserInput()) {
            return Optional.ofNullable(defaultOption);
        }

        options = Optional.ofNullable(options)
            .map(List::stream)
            .orElse(Stream.empty())
            .collect(Collectors.toList());

        int defaultIndex = getDefaultIndex(options, defaultOption);
        Optional<String> menu = toMenu(options, defaultIndex, namedInput);

        if(!menu.isPresent()) {
            return Optional.ofNullable(defaultOption);
        }

        Optional<String> input = readLine(menu.get());
        int selectedIndex = input
            .filter(CommonUtils::isNotBlank)
            .filter(CommonUtils::isNumeric)
            .map(Integer::parseInt)
            .orElse(defaultIndex);
        int optionIndex = selectedIndex - START_INDEX;

        try {
            return Optional.ofNullable(options.get(optionIndex));
        } catch(IndexOutOfBoundsException ex) {
            throw new ConsoleInputException("Invalid option index [" + input.orElse(null) + "]");
        }
    }

    private int getDefaultIndex(List<String> options, String defaultOption) {
        if(defaultOption == null) {
            return INVALID_INDEX;
        }

        int index = START_INDEX;
        defaultOption = trim(defaultOption);

        for(String option : options) {
            if(defaultOption.equalsIgnoreCase(trim(option))) {
                return index;
            }
            index++;
        }

        return INVALID_INDEX;
    }

    private Optional<String> toMenu(List<String> options, int defaultIndex, String namedInput) {
        if(isEmpty(options)) {
            return Optional.empty();
        }

        List<String> formattedOptions = formatAsOrderedList(options,
            (i, opt) -> i == defaultIndex, optionDecorator);
        StringBuilder menu = formatInColumns(settings, formattedOptions);

        String optionMessage = isNotBlank(namedInput)
            ? String.format(OPTION_INPUT_NAME, namedInput)
            : OPTION_INPUT;

        menu.append(optionMessage);

        return Optional.of(menu.toString());
    }

    protected Optional<String> readLine(String message) {
        return Optional.ofNullable(System.console())
            .map(console -> console.readLine(message));
    }

    protected Optional<char[]> readPassword(String message) {
        return Optional.ofNullable(System.console())
            .map(console -> console.readPassword(message));
    }
}
