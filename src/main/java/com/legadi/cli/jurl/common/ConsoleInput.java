package com.legadi.cli.jurl.common;

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

    public static final String INPUT_FORMAT = "input: %s> ";
    public static final String INPUT_DEFAULT_FORMAT = "input: %s [default: %s]> ";
    public static final String PASSWORD_FORMAT = "pswrd: %s> ";
    public static final String OPTION_INPUT = "Select an option> ";

    public static final int START_INDEX = 1;
    public static final int INVALID_INDEX = -1;

    private final Settings settings;
    private final List<String> options;
    private final Function<String, String> optionDecorator;

    public ConsoleInput(Settings settings) {
        this(settings, null, null);
    }

    public ConsoleInput(Settings settings, List<String> options,
            Function<String, String> optionDecorator) {
        this.settings = settings;
        this.options = Optional.ofNullable(options)
            .map(List::stream)
            .orElse(Stream.empty())
            .collect(Collectors.toList());
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

    public Optional<String> selectOption(String defaultOption) {
        if(settings.isSkipUserInput()) {
            return Optional.ofNullable(defaultOption);
        }

        int defaultIndex = getDefaultIndex(defaultOption);
        Optional<String> menu = toMenu(defaultIndex);

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

    private int getDefaultIndex(String defaultOption) {
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

    private Optional<String> toMenu(int defaultIndex) {
        if(isEmpty(options)) {
            return Optional.empty();
        }

        StringBuilder menu = new StringBuilder();
        int index = START_INDEX;

        for(String option : options) {
            menu
                .append(index)
                .append(") ")
                .append(index == defaultIndex
                    ? "(default) " : "")
                .append(optionDecorator.apply(option))
                .append("\n");

            index++;
        }

        menu.append(OPTION_INPUT);

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
