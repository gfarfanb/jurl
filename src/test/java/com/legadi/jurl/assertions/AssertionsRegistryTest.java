package com.legadi.jurl.assertions;

import static com.legadi.jurl.assertions.AssertionsRegistry.findByName;
import static com.legadi.jurl.assertions.AssertionsRegistry.registerAssertionFunction;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.AssertionException;
import com.legadi.jurl.exception.CommandException;

public class AssertionsRegistryTest {

    @Test
    public void registerAssertionCustom() {
        registerAssertionFunction(TestAssertion.class.getName());

        Assertions.assertDoesNotThrow(() -> (TestAssertion) findByName("test"));

        AssertionFunction registered = registerAssertionFunction(TestAssertion.class.getName());

        Assertions.assertNotNull(registered);
    }

    @Test
    public void duplicate() {
        Assertions.assertThrows(CommandException.class,
            () -> registerAssertionFunction(ContainsAssertionFunction::new));
    }

    @Test
    public void notFound() {
        Assertions.assertThrows(CommandException.class,
            () -> findByName("not-found"));
    }

    public static class TestAssertion implements AssertionFunction {

        @Override
        public String name() {
            return "test";
        }

        @Override
        public String[] getArgs() {
            return new String[0];
        }

        @Override
        public boolean apply(String[] args) throws AssertionException {
            return true;
        }
    }
}
