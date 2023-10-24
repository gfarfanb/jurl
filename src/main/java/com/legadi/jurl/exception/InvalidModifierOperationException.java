package com.legadi.jurl.exception;

public class InvalidModifierOperationException extends Exception {

    private final String operation;

    public InvalidModifierOperationException(String operation) {
        this.operation = operation;
    }

    public String getOperation() {
        return operation;
    }
}
