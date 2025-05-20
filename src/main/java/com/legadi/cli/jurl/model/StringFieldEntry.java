package com.legadi.cli.jurl.model;

import java.util.Set;

public interface StringFieldEntry {

    void putField(String fieldName, String value);

    String getField(String fieldName);

    Set<String> getFieldNames();
}
