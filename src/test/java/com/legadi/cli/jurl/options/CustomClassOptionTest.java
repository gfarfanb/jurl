package com.legadi.cli.jurl.options;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.assertions.AssertionFunction;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.CommandException;

public class CustomClassOptionTest extends OptionAbstractTest<CustomClassOption> {

    public CustomClassOptionTest() {
        super("--custom-class");
    }

    @Test
    public void registerClassValidation() throws InterruptedException {
        Settings settings = new Settings();

        Assertions.assertDoesNotThrow(
            () -> jurlOpts(settings,
                "-cc", CustomAssertionFunction.class.getName(),
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));
    }

    @Test
    public void invalidGroupClass() {
        Settings settings = new Settings();

        Assertions.assertThrows(CommandException.class,
            () -> jurlOpts(settings,
                "-cc", UnexpectedClass.class.getName(),
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));
    }

    public static class CustomAssertionFunction implements AssertionFunction {

        @Override
        public String name() {
            return "CUSTOM";
        }

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public boolean apply(String[] args) {
            return true;
        }
    }

    public static class UnexpectedClass {

    }
}
