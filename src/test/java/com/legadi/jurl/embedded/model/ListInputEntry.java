package com.legadi.jurl.embedded.model;

import java.math.BigDecimal;

public class ListInputEntry {

    private String name;
    private String type;
    private BigDecimal value;
    private ListInputPropertyEntries properties;

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

    public ListInputPropertyEntries getProperties() {
        return properties;
    }

    public void setProperties(ListInputPropertyEntries properties) {
        this.properties = properties;
    }
}
