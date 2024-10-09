package com.legadi.cli.jurl.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class FlowEntryTest {

    @Test
    public void setterGetterValidation() {
        FlowEntry model = new FlowEntry();

        Map<String, Object> defaults = new HashMap<>();
        defaults.put("default.int", "5");
        model.setDefaults(defaults);

        List<StepEntry> steps = new ArrayList<>();
        steps.add(new StepEntry());
        model.setSteps(steps);

        Assertions.assertNotNull(model.getDefaults());
        Assertions.assertFalse(model.getDefaults().isEmpty());
        Assertions.assertEquals("5", model.getDefaults().get("default.int"));
        Assertions.assertNotNull(model.getSteps());
        Assertions.assertFalse(model.getSteps().isEmpty());
    }
}
