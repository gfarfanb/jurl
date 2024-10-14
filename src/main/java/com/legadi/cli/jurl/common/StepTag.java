package com.legadi.cli.jurl.common;

public class StepTag {

    private final String requestInputPath;
    private final String flowName;
    private final int index;
    private final int stepsCount;

    public StepTag(String requestInputPath, String flowName, int index, int stepsCount) {
        this.requestInputPath = requestInputPath;
        this.flowName = flowName;
        this.index = index;
        this.stepsCount = stepsCount;
    }

    public String getFlowLabel() {
        return requestInputPath + "/" + flowName;
    }

    public String getStepLabel() {
        return index + "/" + stepsCount;
    }
}
