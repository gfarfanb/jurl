package com.legadi.jurl.model;

import java.time.LocalDateTime;

public class HistoryEntry {

    private String curl;
    private LocalDateTime timestamp;
    private String executionTag;
    private long nanoTime;
    private int assertions;
    private int failures;

    public String getCurl() {
        return curl;
    }

    public void setCurl(String curl) {
        this.curl = curl;
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(LocalDateTime timestamp) {
        this.timestamp = timestamp;
    }

    public String getExecutionTag() {
        return executionTag;
    }

    public void setExecutionTag(String executionTag) {
        this.executionTag = executionTag;
    }

    public long getNanoTime() {
        return nanoTime;
    }

    public void setNanoTime(long nanoTime) {
        this.nanoTime = nanoTime;
    }

    public int getAssertions() {
        return assertions;
    }

    public void setAssertions(int assertions) {
        this.assertions = assertions;
    }

    public int getFailures() {
        return failures;
    }

    public void setFailures(int failures) {
        this.failures = failures;
    }
}
