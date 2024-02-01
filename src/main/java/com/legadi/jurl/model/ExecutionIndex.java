package com.legadi.jurl.model;

public class ExecutionIndex {

    private final int index;
    private final int number;
    private final int times;

    public ExecutionIndex(int index, int number, int times) {
        this.index = index;
        this.number = number;
        this.times = times;
    }

    public int getIndex() {
        return index;
    }

    public int getNumber() {
        return number;
    }

    public int getTimes() {
        return times;
    }

    @Override
    public String toString() {
        return number + "/" + times;
    }
}
