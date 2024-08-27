package com.legadi.cli.jurl.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class InvalidInputEntryExceptionTest {

    @Test
    public void invalidInputEntryException() {
        try {
            throw new InvalidInputEntryException("Invalid input");
        } catch(RuntimeException ex) {
            Assertions.assertDoesNotThrow(() -> (InvalidInputEntryException) ex);
            Assertions.assertEquals("Invalid input", ex.getMessage());
        }
    }
}
