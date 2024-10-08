package com.legadi.cli.jurl.model;

public class AssertionEntry {

    private AssertionType type;
    private String name;
    private String assertionClass;
    private String message;
    private String[] args = new String[0];

    public AssertionType getType() {
        return type;
    }

    public void setType(AssertionType type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAssertionClass() {
        return assertionClass;
    }

    public void setAssertionClass(String assertionClass) {
        this.assertionClass = assertionClass;
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
