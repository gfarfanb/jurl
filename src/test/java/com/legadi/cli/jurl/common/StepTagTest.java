package com.legadi.cli.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class StepTagTest {

    @Test
    public void getFlowLabelValidation() {
        StepTag stepTag = new StepTag("spec.http", "flowName",  1, 2);

        Assertions.assertEquals("spec.http/flowName", stepTag.getFlowLabel());
    }

    @Test
    public void getStepLabelValidation() {
        StepTag stepTag = new StepTag("spec.http", "flowName",  1, 2);

        Assertions.assertEquals("1/2", stepTag.getStepLabel());
    }
}
