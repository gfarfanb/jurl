package com.legadi.jurl.model;

public class AssertionEntry {

    private AssertionType type;
    private String assertClass;
    private String message;
    private String[] args;

    public AssertionType getType() {
        return type;
    }

    public void setType(AssertionType type) {
        this.type = type;
    }

    public String getAssertClass() {
        return assertClass;
    }

    public void setAssertClass(String assertClass) {
        this.assertClass = assertClass;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String[] getArgs() {
        return args;
    }

    public void setArgs(String[] args) {
        this.args = args;
    }
}
