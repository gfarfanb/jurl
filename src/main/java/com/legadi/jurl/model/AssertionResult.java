package com.legadi.jurl.model;

public class AssertionResult {

    private final int assertions;

    private int failures;
    private boolean skip;

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

    public boolean isSkip() {
        return skip;
    }

    public void setSkip(boolean skip) {
        this.skip = skip;
    }
}
