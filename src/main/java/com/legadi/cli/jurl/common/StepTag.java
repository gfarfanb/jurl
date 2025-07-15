package com.legadi.cli.jurl.common;

import com.legadi.cli.jurl.model.ExecutionIndex;

public class StepTag {

    private final String requestInputPath;
    private final String flowName;
    private final ExecutionIndex executionIndex;
    private final int stepIndex;
    private final int stepsCount;

    private String stepName;
    private boolean semaphore;

    public StepTag(String requestInputPath, String flowName,
            ExecutionIndex executionIndex, int stepIndex, int stepsCount,
            boolean semaphore) {
        this.requestInputPath = requestInputPath;
        this.flowName = flowName;
        this.executionIndex = executionIndex;
        this.stepIndex = stepIndex;
        this.stepsCount = stepsCount;
        this.semaphore = semaphore;
    }

    public ExecutionIndex getExecutionIndex() {
        return executionIndex;
    }

    public int getStepIndex() {
        return stepIndex;
    }

    public String getStepName() {
        return stepName;
    }

    public void setStepName(String stepName) {
        this.stepName = stepName;
    }

    public boolean isSemaphore() {
        return semaphore;
    }

    public void setSemaphore(boolean semaphore) {
        this.semaphore = semaphore;
    }

    public String getFlowLabel() {
        return requestInputPath + "/" + flowName;
    }

    public String getStepLabel() {
        return stepIndex + "/" + stepsCount
            + (stepName != null ? ": " + stepName : "");
    }
}
