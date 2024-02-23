package com.legadi.jurl.model;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class AssertionResultTest {

    @Test
    public void setterGetterValidation() {
        AssertionResult model = new AssertionResult(5);

        model.setFailures(3);
        model.setPassed(false);
        model.getFailedMessages().add("Failure 1");
        model.getFailedMessages().add("Failure 2");
        model.getFailedMessages().add("Failure 3");

        Assertions.assertEquals(5, model.getAssertions());
        Assertions.assertEquals(3, model.getFailures());
        Assertions.assertEquals(false, model.isPassed());
        Assertions.assertTrue(model.getFailedMessages().stream().allMatch(msg -> msg.startsWith("Failure")));
        Assertions.assertEquals(3, model.getFailedMessages().size());
    }
}
