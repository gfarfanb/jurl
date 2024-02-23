package com.legadi.jurl.model;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ExecutionIndexTest {

    @Test
    public void setterGetterValidation() {
        ExecutionIndex model = new ExecutionIndex(0, 1, 2);

        Assertions.assertEquals(0, model.getIndex());
        Assertions.assertEquals(1, model.getNumber());
        Assertions.assertEquals(2, model.getTimes());
        Assertions.assertEquals("1/2", model.toString());
    }
}
