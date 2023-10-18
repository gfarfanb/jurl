package com.legadi.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ExecutionLevelsTest {

    @Test
    public void validateNoLevel() {
        ExecutionLevels executionLevels = new ExecutionLevels();

        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test"));
    }

    @Test
    public void validateFirstLevel() {
        ExecutionLevels executionLevels = new ExecutionLevels();

        executionLevels.nextLevel();

        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-1"));
        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-1"));
    }

    @Test
    public void validateSecondLevel() {
        ExecutionLevels executionLevels = new ExecutionLevels();

        executionLevels.nextLevel();

        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-1"));

        executionLevels.nextLevel();

        Assertions.assertTrue(executionLevels.wasExecuted("path/test", "test-1"));
    }

    @Test
    public void validateThirdLevel() {
        ExecutionLevels executionLevels = new ExecutionLevels();

        executionLevels.nextLevel();

        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-1"));

        executionLevels.nextLevel();

        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-2.1"));
        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-2.2"));

        executionLevels.nextLevel();

        Assertions.assertFalse(executionLevels.wasExecuted("path/test", "test-3"));
        Assertions.assertTrue(executionLevels.wasExecuted("path/test", "test-1"));
    }

    @Test
    public void getTraceValidation() {
        ExecutionLevels executionLevels = new ExecutionLevels();
        String expectedTrace = "[path/test/test-1]\n"
            + "    -> [path/test/test-2]\n"
            + "        -> [path/test/test-3]\n";

        executionLevels.nextLevel();
        executionLevels.wasExecuted("path/test", "test-1");
        executionLevels.nextLevel();
        executionLevels.wasExecuted("path/test", "test-2");
        executionLevels.nextLevel();
        executionLevels.wasExecuted("path/test", "test-3");

        Assertions.assertEquals(expectedTrace, executionLevels.getTrace());
    }
}
