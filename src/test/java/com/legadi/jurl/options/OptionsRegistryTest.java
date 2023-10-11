package com.legadi.jurl.options;

import static com.legadi.jurl.options.OptionsRegistry.findByArg;
import static com.legadi.jurl.options.OptionsRegistry.getAddOns;
import static com.legadi.jurl.options.OptionsRegistry.getOptions;
import static com.legadi.jurl.options.OptionsRegistry.registerAddOn;
import static com.legadi.jurl.options.OptionsRegistry.registerOption;

import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class OptionsRegistryTest {

    @Test
    public void registerAddOnCustom() {
        registerAddOn(TestOption.class.getName());

        Assertions.assertDoesNotThrow(() -> (TestOption) findByArg("++test"));
    }

    @Test
    public void duplicate() {
        registerOption(() -> new TestOption("**test", "*t"), true);

        Assertions.assertThrows(CommandException.class,
            () -> registerOption(() -> new TestOption("--help", null)));

        Assertions.assertThrows(CommandException.class,
            () -> registerOption(() -> new TestOption("++help", "-h")));

        Assertions.assertThrows(CommandException.class,
            () -> registerOption(() -> new TestOption("**test", null)));

        Assertions.assertThrows(CommandException.class,
            () -> registerOption(() -> new TestOption("~~test", "*t")));
    }

    @Test
    public void blankOpt() {
        Assertions.assertThrows(CommandException.class,
            () -> registerOption(() -> new TestOption(null, null)));
    }

    @Test
    public void getOptionsSet() {
        Set<Option> options = getOptions();

        Assertions.assertFalse(options.isEmpty());
        Assertions.assertTrue(options.stream().anyMatch(option -> option.getOpt().equals("--help")));
    }

    @Test
    public void getAddOnsSet() {
        registerOption(() -> new TestOption("??test", null), true);

        Set<Option> addOns = getAddOns();

        Assertions.assertFalse(addOns.isEmpty());
        Assertions.assertTrue(addOns.stream().anyMatch(option -> option.getOpt().equals("??test")));
    }

    @Test
    public void notFound() {
        Option option = Assertions.assertDoesNotThrow(
            () -> findByArg("not-found"));

        Assertions.assertNull(option);
    }

    public static class TestOption extends Option {

        private final String opt;
        private final String alias;

        public TestOption() {
            this("++test", null);
        }

        public TestOption(String opt, String alias) {
            this.opt = opt;
            this.alias = alias;
        }

        @Override
        public String getOpt() {
            return opt;
        }

        @Override
        public String getAlias() {
            return alias;
        }

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public String getDescription() {
            return "Test";
        }

        @Override
        public boolean execute(Settings settings, String[] args) {
            return true;
        }
    }
}
