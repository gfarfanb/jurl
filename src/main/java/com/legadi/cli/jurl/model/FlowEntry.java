package com.legadi.cli.jurl.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FlowEntry {

    private String name;
    private String description;
    private Map<String, Object> defaults = new HashMap<>();
    private List<StepEntry> steps = new ArrayList<>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Map<String, Object> getDefaults() {
        return defaults;
    }

    public void setDefaults(Map<String, Object> defaults) {
        this.defaults = defaults;
    }

    public List<StepEntry> getSteps() {
        return steps;
    }

    public void setSteps(List<StepEntry> steps) {
        this.steps = steps;
    }
}
