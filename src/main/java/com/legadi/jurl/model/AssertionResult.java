package com.legadi.jurl.model;

public class AssertionResult {

    private final int assertions;

    private int failures;
    private boolean passed = true;

    public AssertionResult(int assertions) {
        this.assertions = assertions;
    }

    public int getAssertions() {
        return assertions;
    }

    public int getFailures() {
        return failures;
    }

    public void setFailures(int failures) {
        this.failures = failures;
    }

    public boolean isPassed() {
        return passed;
    }

    public void setPassed(boolean passed) {
        this.passed = passed;
    }
}
