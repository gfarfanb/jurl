package com.legadi.jurl.common;

import java.time.LocalDateTime;

import static java.time.temporal.ChronoField.MILLI_OF_DAY;

public class ExecutionTag {

    private final LocalDateTime currentExecution;

    public ExecutionTag() {
        this.currentExecution = LocalDateTime.now();
    }

    public LocalDateTime getCurrentExecution() {
        return currentExecution;
    }

    @Override
    public String toString() {
        return currentExecution.toLocalDate() + "." + currentExecution.toLocalTime().getLong(MILLI_OF_DAY);
    }
}
