package com.legadi.cli.jurl.common.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Evaluable {

    String[] values();

    Operation op();

    public enum Operation {

        EQUALS,
        EQUALS_IGNORE_CASE,
        CONTAINS,
        STARTS_WITH,
        ENDS_WITH,
        ALWAYS_TRUE,
        NOTHING
    }
}
