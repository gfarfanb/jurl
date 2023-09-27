package com.legadi.jurl.model.http;

import java.util.HashMap;
import java.util.Map;

import com.google.gson.annotations.Expose;

public class HTTPRequestFileEntry {

    private String name;
    private String path;
    private String field;
    private String mineType;
    private Map<String, String> formData = new HashMap<>();
    @Expose(serialize = false, deserialize = false)
    private String boundary;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public String getMineType() {
        return mineType;
    }

    public void setMineType(String mineType) {
        this.mineType = mineType;
    }

    public Map<String, String> getFormData() {
        return formData;
    }

    public void setFormData(Map<String, String> formData) {
        this.formData = formData;
    }

    public String getBoundary() {
        return boundary;
    }

    public void setBoundary(String boundary) {
        this.boundary = boundary;
    }
}
