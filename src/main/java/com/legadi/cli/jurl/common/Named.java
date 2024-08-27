package com.legadi.cli.jurl.common;

public interface Named {

    String name();

    default String alias() {
        return null;
    }

    boolean allowOverride();
}
