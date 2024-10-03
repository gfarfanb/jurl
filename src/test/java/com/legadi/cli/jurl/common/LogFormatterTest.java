package com.legadi.cli.jurl.common;

import static java.util.logging.Level.INFO;

import java.io.IOException;
import java.util.logging.LogRecord;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class LogFormatterTest {

    @Test
    public void formatValidation() throws IOException {
        LogRecord logRecord = new LogRecord(INFO, "Test");
        LogFormatter logFormatter = new LogFormatter();

        Assertions.assertEquals("Test\n", logFormatter.format(logRecord));

        logRecord.setThrown(new Exception());

        Assertions.assertTrue(logFormatter.format(logRecord).contains(Exception.class.getName()));
    }
}
