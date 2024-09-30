package com.legadi.cli.jurl.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FlowEntry {

    private Map<String, String> defaults = new HashMap<>();
    private List<StepEntry> steps = new ArrayList<>();

    public Map<String, String> getDefaults() {
        return defaults;
    }

    public void setDefaults(Map<String, String> defaults) {
        this.defaults = defaults;
    }

    public List<StepEntry> getSteps() {
        return steps;
    }

    public void setSteps(List<StepEntry> steps) {
        this.steps = steps;
    }
}
