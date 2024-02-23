package com.legadi.jurl.options;

import static com.legadi.jurl.common.ObjectsRegistry.findByNameOrFail;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.assertions.AssertionFunction;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class CustomClassOptionTest extends OptionTest<CustomClassOption> {

    public CustomClassOptionTest() {
        super("--custom-class");
    }

    @Test
    public void registerClassValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-cc", CustomAssertionFunction.class.getName(),
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        AssertionFunction assertionFunction = Assertions.assertDoesNotThrow(
            () -> findByNameOrFail(AssertionFunction.class, "CUSTOM"));

        Assertions.assertTrue(assertionFunction instanceof CustomAssertionFunction);

        HTTPResponseEntry response = requestCatcher.getLast(correlationId, "response");

        Assertions.assertEquals(201, response.getStatusCode());
    }

    @Test
    public void invalidGroupClass() {
        Assertions.assertThrows(CommandException.class,
            () -> jurl(
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
