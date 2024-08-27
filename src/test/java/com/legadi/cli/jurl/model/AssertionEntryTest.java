package com.legadi.cli.jurl.model;

import static com.legadi.cli.jurl.model.AssertionType.ASSERTION;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.assertions.EqualsToAssertionFunction;

public class AssertionEntryTest {

    @Test
    public void setterGetterValidation() {
        AssertionEntry model = new AssertionEntry();

        model.setType(ASSERTION);
        model.setName("EQUALS_TO");
        model.setAssertionClass(EqualsToAssertionFunction.class.getName());
        model.setMessage("Message if evaluation fails");
        model.setArgs(new String[] { "a", "A" });

        Assertions.assertEquals(ASSERTION, model.getType());
        Assertions.assertEquals("EQUALS_TO", model.getName());
        Assertions.assertEquals(EqualsToAssertionFunction.class.getName(), model.getAssertionClass());
        Assertions.assertEquals("Message if evaluation fails", model.getMessage());
        Assertions.assertNotNull(model.getArgs());
        Assertions.assertEquals(2, model.getArgs().length);
    }
}
