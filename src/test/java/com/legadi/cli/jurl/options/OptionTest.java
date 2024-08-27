package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

import java.util.Objects;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPITest;

public abstract class OptionTest<T extends Option> extends EmbeddedAPITest {

    protected final T option;

    @SuppressWarnings("unchecked")
    public OptionTest(String optionName) {
        this.option = (T) findByNameOrFail(Option.class, optionName);
    }

    @Test
    public void validateDefinitionMethods() {
        Assertions.assertNotNull(option.getDescription());
        Assertions.assertDoesNotThrow(() -> Boolean.valueOf(option.allowedForRequestAuth()));
        Assertions.assertDoesNotThrow(() -> Integer.valueOf(option.getPriority()));
        Assertions.assertEquals(Objects.hash(option.name(), option.alias()), option.hashCode());
        Assertions.assertTrue(option.equals(option));
        Assertions.assertFalse(option.equals(new Object()));
        Assertions.assertTrue(option.equals(new NamedOption(option.name(), option.alias())));
        Assertions.assertFalse(option.equals(new NamedOption(UUID.randomUUID().toString(), null)));
        Assertions.assertFalse(option.equals(new NamedOption(option.name(), UUID.randomUUID().toString())));
        Assertions.assertTrue(option.toString().startsWith(option.name() + ", " + option.alias()));
    }

    public static class NamedOption extends Option {

        private final String name;
        private final String alias;

        public NamedOption(String name, String alias) {
            this.name = name;
            this.alias = alias;
        }

        @Override
        public String name() {
            return name;
        }

        @Override
        public String alias() {
            return alias;
        }

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public String getDescription() {
            return null;
        }

        @Override
        public boolean execute(Settings settings, String[] args) {
            return true;
        }
    }
}
