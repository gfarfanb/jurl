package com.legadi.cli.jurl.embedded.model;

import java.math.BigDecimal;
import java.util.List;

public class ObjectInput {

    private String name;
    private String type;
    private BigDecimal value;
    private List<ObjectInputLogEntry> log;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public BigDecimal getValue() {
        return value;
    }

    public void setValue(BigDecimal value) {
        this.value = value;
    }

    public List<ObjectInputLogEntry> getLog() {
        return log;
    }

    public void setLog(List<ObjectInputLogEntry> log) {
        this.log = log;
    }
}
