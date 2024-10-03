package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;

import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.assertions.AssertionFunction;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class CustomClassOptionTest extends OptionAbstractTest<CustomClassOption> {

    public CustomClassOptionTest() {
        super("--custom-class");
    }

    @Test
    public void registerClassValidation() throws InterruptedException {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-cc", CustomAssertionFunction.class.getName(),
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        AssertionFunction assertionFunction = Assertions.assertDoesNotThrow(
            () -> findByNameOrFail(AssertionFunction.class, "CUSTOM"));

        Assertions.assertTrue(assertionFunction instanceof CustomAssertionFunction);

        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

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
