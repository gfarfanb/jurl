package com.legadi.cli.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import com.legadi.cli.jurl.model.ExecutionIndex;

public class StepTagTest {

    @Test
    public void setterGetterValidation() {
        ExecutionIndex executionIndex = new ExecutionIndex(1, 1, 1);
        StepTag stepTag = new StepTag("spec.http", "flowName", executionIndex, 1, 2, false);

        Assertions.assertNotNull(stepTag.getExecutionIndex());
        Assertions.assertEquals(1, stepTag.getExecutionIndex().getIndex());
        Assertions.assertEquals(1, stepTag.getExecutionIndex().getNumber());
        Assertions.assertEquals(1, stepTag.getExecutionIndex().getTimes());
        Assertions.assertEquals(1, stepTag.getStepIndex());
        Assertions.assertNull(stepTag.getStepName());
        Assertions.assertFalse(stepTag.isSemaphore());
        Assertions.assertEquals("spec.http/flowName", stepTag.getFlowLabel());
        Assertions.assertEquals("1/2", stepTag.getStepLabel());

        stepTag.setStepName("test");
        stepTag.setSemaphore(true);

        Assertions.assertEquals("test", stepTag.getStepName());
        Assertions.assertTrue(stepTag.isSemaphore());
        Assertions.assertEquals("1/2: test", stepTag.getStepLabel());
    }
}
