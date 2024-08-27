package com.legadi.cli.jurl.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class RecursiveCommandExceptionTest {

    @Test
    public void recursiveCommandException() {
        try {
            throw new RecursiveCommandException("Same call");
        } catch(RuntimeException ex) {
            Assertions.assertDoesNotThrow(() -> (RecursiveCommandException) ex);
            Assertions.assertEquals("Same call", ex.getMessage());
        }
    }
}
