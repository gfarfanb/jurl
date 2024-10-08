package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.SettingsConstants.PROP_SKIP_USER_INPUT;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.ConsoleInputException;

public class ConsoleInputTest {

    @Test
    public void readLineDefaultImplementation() {
        Settings settings = new Settings();
        ConsoleInput consoleInput = new ConsoleInput(settings);

        if(System.console() == null) {
            Assertions.assertFalse(consoleInput.readLine("User input").isPresent());
        } else {
            Assertions.assertNotNull(consoleInput);
        }
    }

    @Test
    public void readPasswordImplementation() {
        Settings settings = new Settings();
        ConsoleInput consoleInput = new ConsoleInput(settings);

        if(System.console() == null) {
            Assertions.assertFalse(consoleInput.readPassword("Password").isPresent());
        } else {
            Assertions.assertNotNull(consoleInput);
        }
    }

    @Test
    public void selectOptionImplementation() {
        Settings settings = new Settings();
        List<String> options = Arrays.asList("Option 1", "Option 2", "Option 3");
        ConsoleInput consoleInput = new ConsoleInput(settings, options, null);

        if(System.console() == null) {
            Assertions.assertTrue(consoleInput.selectOption("Option 1").isPresent());
        } else {
            Assertions.assertNotNull(consoleInput);
        }

        ConsoleInput consoleInputDecorator = new ConsoleInput(settings, options, opt -> opt);

        if(System.console() == null) {
            Assertions.assertThrows(ConsoleInputException.class,
                () -> consoleInputDecorator.selectOption("Option n"));
        } else {
            Assertions.assertNotNull(consoleInputDecorator);
        }
    }

    @Test
    public void readInputValidation() {
        Settings settings = new Settings();
        String expectedInput = UUID.randomUUID().toString();
        ConsoleInput consoleInput = new ConsoleInputDummy(expectedInput, settings);
        String input;

        input = consoleInput.readInput("User input", null);

        Assertions.assertEquals(expectedInput, input);

        input = consoleInput.readInput("User input", "default");

        Assertions.assertEquals(expectedInput, input);
    }

    @Test
    public void readInputSkipped() {
        Settings settings = new Settings();
        String expectedInput = UUID.randomUUID().toString();
        ConsoleInput consoleInput = new ConsoleInputDummy(expectedInput, settings);

        settings.putUserInput(PROP_SKIP_USER_INPUT, Boolean.TRUE.toString());

        String input = consoleInput.readInput("User input", expectedInput);

        Assertions.assertEquals(expectedInput, input);
    }

    @Test
    public void readInputWithDefault() {
        Settings settings = new Settings();
        String expectedInput = UUID.randomUUID().toString();
        ConsoleInput consoleInput = new ConsoleInputDummy(null, settings);

        String input = consoleInput.readInput("User input", expectedInput);

        Assertions.assertEquals(expectedInput, input);
    }

    @Test
    public void readPasswordValidation() {
        Settings settings = new Settings();
        String expectedPassword = UUID.randomUUID().toString();
        ConsoleInput consoleInput = new ConsoleInputDummy(expectedPassword, settings);
        String password;

        password = consoleInput.readPassword("Password", null);

        Assertions.assertEquals(expectedPassword, password);

        password = consoleInput.readPassword("Password", "d9f6u17");

        Assertions.assertEquals(expectedPassword, password);
    }

    @Test
    public void readPasswordSkipped() {
        Settings settings = new Settings();
        String expectedPassword = UUID.randomUUID().toString();
        ConsoleInput consoleInput = new ConsoleInputDummy(expectedPassword, settings);

        settings.putUserInput(PROP_SKIP_USER_INPUT, Boolean.TRUE.toString());

        String password = consoleInput.readPassword("Password", expectedPassword);

        Assertions.assertEquals(expectedPassword, password);
    }

    @Test
    public void readPasswordWithDefault() {
        Settings settings = new Settings();
        String expectedPassword = UUID.randomUUID().toString();
        ConsoleInput consoleInput = new ConsoleInputDummy(null, settings);

        String password = consoleInput.readPassword("Password", expectedPassword);

        Assertions.assertEquals(expectedPassword, password);
    }

    @Test
    public void selectOptionValidation() {
        Settings settings = new Settings();
        String expectedInput = "1";
        List<String> options = Arrays.asList("Option 1", "Option 2");
        int columnLength = settings.getConsoleWidth() / (options.size() + 1);
        ConsoleInput consoleInput = new ConsoleInputDummy(expectedInput,
            settings, options, opt -> String.format("%-" + columnLength + "s", opt));
        Optional<String> option;

        option = consoleInput.selectOption(null);

        Assertions.assertTrue(option.isPresent());
        Assertions.assertEquals("Option 1", option.get());

        option = consoleInput.selectOption("Option 2");

        Assertions.assertTrue(option.isPresent());
        Assertions.assertEquals("Option 1", option.get());
    }

    @Test
    public void selectOptionSkipped() {
        Settings settings = new Settings();
        String expectedInput = "Option 1";
        List<String> options = Arrays.asList("Option 1", "Option 2", "Option 3");
        ConsoleInput consoleInput = new ConsoleInputDummy(expectedInput,
            settings, options, null);

        settings.putUserInput(PROP_SKIP_USER_INPUT, Boolean.TRUE.toString());

        Optional<String> option = consoleInput.selectOption(expectedInput);

        Assertions.assertTrue(option.isPresent());
        Assertions.assertEquals(expectedInput, option.get());
    }

    @Test
    public void selectOptionWithDefault() {
        Settings settings = new Settings();
        List<String> options = Arrays.asList("Option 1", "Option 2", "Option 3");
        int columnLength = settings.getConsoleWidth() / options.size();
        ConsoleInput consoleInput = new ConsoleInputDummy(null,
            settings, options, opt -> String.format("%-" + columnLength + "s", opt));

        Optional<String> option = consoleInput.selectOption("Option 2");

        Assertions.assertTrue(option.isPresent());
        Assertions.assertEquals("Option 2", option.get());
    }

    @Test
    public void selectOptionFromEmpty() {
        Settings settings = new Settings();
        String expectedInput = "Option 1";
        ConsoleInput consoleInput = new ConsoleInputDummy(null, settings, null, null);

        Optional<String> option = consoleInput.selectOption(expectedInput);

        Assertions.assertTrue(option.isPresent());
        Assertions.assertEquals(expectedInput, option.get());
    }

    @Test
    public void selectOptionInvalid() {
        Settings settings = new Settings();
        String expectedInput = "Option n";
        List<String> options = Arrays.asList("Option 1", "Option 2", "Option 3");
        ConsoleInput consoleInput = new ConsoleInputDummy(null,
            settings, options, opt -> String.format("%-" + settings.getConsoleWidth() + "s", opt));

        Assertions.assertThrows(ConsoleInputException.class,
            () -> consoleInput.selectOption(expectedInput));
    }

    public static class ConsoleInputDummy extends ConsoleInput {

        private final String input;

        public ConsoleInputDummy(String input, Settings settings) {
            super(settings);
            this.input = input;
        }

        public ConsoleInputDummy(String input, Settings settings,
                List<String> options, Function<String, String> optionDecorator) {
            super(settings, options, optionDecorator);
            this.input = input;
        }

        @Override
        protected Optional<String> readLine(String message) {
            return Optional.ofNullable(input);
        }

        @Override
        protected Optional<char[]> readPassword(String message) {
            return Optional.ofNullable(input)
                .map(String::toCharArray);
        }
    }
}
