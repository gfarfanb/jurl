package com.legadi.cli.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.RecursiveCommandException;

public class ExecutionTraceTest {

    @Test
    public void validateOneCall() {
        Settings settings = new Settings();
        ExecutionTrace trace = new ExecutionTrace(settings);

        Assertions.assertDoesNotThrow(
            () -> trace.validateExecution("path/test", "test"));
    }

    @Test
    public void validateSeveralCalls() {
        Settings settings = new Settings();
        ExecutionTrace trace = new ExecutionTrace(settings);

        Assertions.assertDoesNotThrow(
            () -> trace.validateExecution("path/test", "test-1"));
        Assertions.assertThrows(RecursiveCommandException.class,
            () -> trace.validateExecution("path/test", "test-1"));
    }

    @Test
    public void validateDifferentTraces() {
        Settings settings = new Settings();
        ExecutionTrace trace = new ExecutionTrace(settings);

        Assertions.assertDoesNotThrow(
            () -> trace.validateExecution("path/test", "test-1"));

        ExecutionTrace trace1 = trace.nextIteration();

        Assertions.assertDoesNotThrow(
            () -> trace1.validateExecution("path/test", "test-2.1"));
        Assertions.assertDoesNotThrow(
            () -> trace1.validateExecution("path/test", "test-2.2"));
        Assertions.assertThrows(RecursiveCommandException.class,
            () -> trace1.validateExecution("path/test", "test-2.1"));

        ExecutionTrace trace2 = trace.nextIteration();

        Assertions.assertDoesNotThrow(
            () -> trace2.validateExecution("path/test", "test-3"));
        Assertions.assertThrows(RecursiveCommandException.class,
            () -> trace2.validateExecution("path/test", "test-1"));
    }
}
