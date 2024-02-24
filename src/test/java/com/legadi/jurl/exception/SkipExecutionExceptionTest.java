package com.legadi.jurl.exception;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class SkipExecutionExceptionTest {

    @Test
    public void skipExecutionException() {
        try {
            throw new SkipExecutionException();
        } catch(RuntimeException ex) {
            Assertions.assertDoesNotThrow(() -> (SkipExecutionException) ex);
        }
    }
}
