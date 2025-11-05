package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractConfigReplaceableProperties;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAlias;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAllowOverride;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractTypedType;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.annotations.ConfigReplaceable;
import com.legadi.cli.jurl.common.annotations.Named;
import com.legadi.cli.jurl.common.annotations.Typed;

public class AnnotationsUtilsTest {

    @Test
    public void validateConfigReplaceable() {
        ConfigReplaceableTest value = new ConfigReplaceableTest();

        Assertions.assertArrayEquals(new String[] { "field" }, extractConfigReplaceableProperties(value));

        Assertions.assertThrows(IllegalStateException.class,
            () -> extractConfigReplaceableProperties(new Object()));
    }

    @Test
    public void validateNamed() {
        NamedTest value = new NamedTest();

        Assertions.assertEquals("named", extractNamedName(value));
        Assertions.assertEquals("test", extractNamedAlias(value));
        Assertions.assertTrue(extractNamedAllowOverride(value));

        Assertions.assertThrows(IllegalStateException.class,
            () -> extractNamedName(new Object()));
        Assertions.assertThrows(IllegalStateException.class,
            () -> extractNamedAlias(new Object()));
        Assertions.assertThrows(IllegalStateException.class,
            () -> extractNamedAllowOverride(new Object()));
    }

    @Test
    public void validateTyped() {
        TypedTest value = new TypedTest();

        Assertions.assertEquals("typed", extractTypedType(value));

        Assertions.assertThrows(IllegalStateException.class,
            () -> extractTypedType(new Object()));
    }

    @ConfigReplaceable(registeredProperties = { "field" })
    public static class ConfigReplaceableTest {

    }

    @Named(name = "named", alias = "test", allowOverride = true)
    public static class NamedTest {

    }

    @Typed(type = "typed")
    public static class TypedTest {

    }
}
