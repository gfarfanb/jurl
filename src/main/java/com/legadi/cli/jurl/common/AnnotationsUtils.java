package com.legadi.cli.jurl.common;

import java.lang.annotation.Annotation;

import com.legadi.cli.jurl.common.annotations.ConfigReplaceable;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.common.annotations.Typed;

public class AnnotationsUtils {

    private AnnotationsUtils() {}

    public static String[] extractConfigReplaceableProperties(Object value) {
        return extractAnnotation(value, ConfigReplaceable.class).registeredProperties();
    }

    public static String extractNamedName(Object value) {
        return extractAnnotation(value, Named.class).name();
    }

    public static String extractNamedAlias(Object value) {
        return extractAnnotation(value, Named.class).alias();
    }

    public static boolean extractNamedAllowOverride(Object value) {
        return extractAnnotation(value, Named.class).allowOverride();
    }

    public static String extractTypedType(Object value) {
        return extractAnnotation(value, Typed.class).type();
    }

    private static <T extends Annotation> T extractAnnotation(Object value, Class<T> annotationClass) {
        T annotation = value.getClass().getAnnotation(annotationClass);
        if(annotation == null) {
            throw new IllegalStateException("Type '" + value.getClass() + "' is not annotated with '" + annotationClass + "'");
        }
        return annotation;
    }
}
