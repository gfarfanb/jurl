package com.legadi.cli.jurl.model;

import static com.legadi.cli.jurl.model.ExecutionStatus.FAILED;
import static com.legadi.cli.jurl.model.ExecutionStatus.PARTIALLY;
import static com.legadi.cli.jurl.model.ExecutionStatus.SKIPPED;
import static com.legadi.cli.jurl.model.ExecutionStatus.SUCCESSFUL;
import static com.legadi.cli.jurl.model.ExecutionStatus.valueOf;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ExecutionStatusTest {

    @Test
    public void valueOfValidation() {
        Assertions.assertEquals(SKIPPED, valueOf("SKIPPED"));
        Assertions.assertEquals(FAILED, valueOf("FAILED"));
        Assertions.assertEquals(SUCCESSFUL, valueOf("SUCCESSFUL"));
        Assertions.assertEquals(PARTIALLY, valueOf("PARTIALLY"));
        Assertions.assertThrows(IllegalArgumentException.class,
            () -> valueOf("UNKNOWN"));
    }
}
