package com.legadi.jurl.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class InvalidAssertionsFoundExceptionTest {

    @Test
    public void invalidAssertionsFoundException() {
        try {
            throw new InvalidAssertionsFoundException();
        } catch(RuntimeException ex) {
            Assertions.assertDoesNotThrow(() -> (InvalidAssertionsFoundException) ex);
        }
    }
}
