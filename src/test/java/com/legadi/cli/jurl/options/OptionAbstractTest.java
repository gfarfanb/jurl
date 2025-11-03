package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

import java.util.Objects;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.embedded.EmbeddedAPIAbstractTest;
import com.legadi.cli.jurl.executor.RequestCommand;

public abstract class OptionAbstractTest<T extends Option> extends EmbeddedAPIAbstractTest {

    protected final T option;
    protected final boolean requiredForAuth;

    @SuppressWarnings("unchecked")
    public OptionAbstractTest(String optionName, boolean requiredForAuth) {
        this.option = (T) findByNameOrFail(Option.class, optionName);
        this.requiredForAuth = requiredForAuth;
    }

    public Settings jurlOpts(String... args) {
        Settings settings = new Settings();
        OptionsReader optionsReader = new OptionsReader(args);
        new RequestCommand(args).executeOptions(settings, optionsReader.getOptionEntries());
        return settings;
    }

    @Test
    public void validateDefinitionMethods() {
        Assertions.assertNotNull(option.getDescription());
        Assertions.assertDoesNotThrow(() -> Integer.valueOf(option.getPriority()));
        Assertions.assertEquals(Objects.hash(option.name(), option.alias()), option.hashCode());
        Assertions.assertTrue(option.equals(option));
        Assertions.assertFalse(option.equals(new Object()));
        Assertions.assertTrue(option.equals(new NamedOption(option.name(), option.alias())));
        Assertions.assertFalse(option.equals(new NamedOption(UUID.randomUUID().toString(), null)));
        Assertions.assertFalse(option.equals(new NamedOption(option.name(), UUID.randomUUID().toString())));
        Assertions.assertTrue(option.toString().startsWith(option.name() + ", " + option.alias()));
        Assertions.assertEquals(requiredForAuth, option.requiredForAuth());
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
