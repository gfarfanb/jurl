package com.legadi.jurl.model;

public enum GeneratorType {

    ALPHA_NUMERIC,
    BOOLEAN,
    DATE_TIME,
    DECIMAL,
    EMAIL,
    FULL_NAME,
    INTEGER,
    LAST_NAME,
    LOREM_IPSUM,
    NAME,
    PASSWORD,
    PICK_ANY,
    INPUT,
    UUID;

    public String tag() {
        return name() + ":";
    }
}
