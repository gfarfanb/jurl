package com.legadi.cli.jurl.model;

import java.util.List;
import java.util.Map;

public class GroupConfig {

    private String active;
    private List<Map<String, String>> collection;

    public String getActive() {
        return active;
    }

    public void setActive(String active) {
        this.active = active;
    }

    public List<Map<String, String>> getCollection() {
        return collection;
    }

    public void setCollection(List<Map<String, String>> collection) {
        this.collection = collection;
    }

}
