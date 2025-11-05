package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

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
    public OptionAbstractTest(Class<T> optionClass, boolean requiredForAuth) {
        this.option = (T) findByNameOrFail(Option.class, getName(optionClass));
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
        Assertions.assertEquals(requiredForAuth, option.requiredForAuth());
        Assertions.assertNotNull(option.getArgs());
        Assertions.assertNotNull(option.getDescription());
        Assertions.assertDoesNotThrow(() -> option.getPriority());
    }

    private String getName(Class<T> optionClass) {
        Named named = optionClass.getAnnotation(Named.class);
        return named.name();
    }
}
