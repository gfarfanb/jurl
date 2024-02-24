package com.legadi.jurl.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CommandExceptionTest {

    @Test
    public void commandException() {
        try {
            throw new CommandException("Invalid definition");
        } catch(RuntimeException ex) {
            Assertions.assertDoesNotThrow(() -> (CommandException) ex);
            Assertions.assertEquals("Invalid definition", ex.getMessage());
        }
    }
}
