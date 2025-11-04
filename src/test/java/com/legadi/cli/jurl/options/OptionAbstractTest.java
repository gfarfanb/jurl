package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAlias;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

import java.util.Objects;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;
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
        Assertions.assertEquals(Objects.hash(extractNamedName(option), extractNamedAlias(option)), option.hashCode());
        Assertions.assertTrue(option.equals(option));
        Assertions.assertFalse(option.equals(new Object()));
        Assertions.assertFalse(option.equals(new NamedOption()));
        Assertions.assertTrue(option.toString().startsWith(extractNamedName(option) + ", " + extractNamedAlias(option)));
        Assertions.assertEquals(requiredForAuth, option.requiredForAuth());
    }

    @Named(name = "lhn345lj6n45lj45ptn3l45knet5", alias = "34jn345j3n4krjn4k3n4")
    public static class NamedOption extends Option {

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
