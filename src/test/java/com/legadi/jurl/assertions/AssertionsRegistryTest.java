package com.legadi.jurl.assertions;

import static com.legadi.jurl.assertions.AssertionsRegistry.registerAssertionFunction;
import static com.legadi.jurl.assertions.AssertionsRegistry.findByName;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;

public class AssertionsRegistryTest {

    @Test
    public void duplicate() {
        Assertions.assertThrows(CommandException.class,
            () -> registerAssertionFunction(ContainsAssertionFunction::new));
    }

    @Test
    public void notFound() {
        Assertions.assertThrows(CommandException.class,
            () -> findByName("NotFound"));
    }
}
