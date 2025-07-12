package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.assertions.AssertionFunction;
import com.legadi.cli.jurl.exception.CommandException;

public class CustomClassOptionTest extends OptionAbstractTest<CustomClassOption> {

    public CustomClassOptionTest() {
        super("--custom-class");
    }

    @Test
    public void registerClassValidation() throws InterruptedException {
        Assertions.assertDoesNotThrow(
            () -> jurlOpts(
                "-cc", CustomAssertionFunction.class.getName(),
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        AssertionFunction assertionFunction = Assertions.assertDoesNotThrow(
            () -> findByNameOrFail(AssertionFunction.class, "CUSTOM"));

        Assertions.assertTrue(assertionFunction instanceof CustomAssertionFunction);
    }

    @Test
    public void invalidGroupClass() {
        Assertions.assertThrows(CommandException.class,
            () -> jurlOpts(
                "-cc", UnexpectedClass.class.getName(),
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));
    }

    public static class CustomAssertionFunction implements AssertionFunction {

        public CustomAssertionFunction() {
            System.out.println(getClass().getName()+ " instantiated");
        }

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
