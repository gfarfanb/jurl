package com.legadi.jurl.model;

public class MockEntry {

    protected int statusCode;
    protected long secondsDelay;

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public long getSecondsDelay() {
        return secondsDelay;
    }

    public void setSecondsDelay(long secondsDelay) {
        this.secondsDelay = secondsDelay;
    }
}
