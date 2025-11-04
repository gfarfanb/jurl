package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAlias;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedAllowOverride;
import static com.legadi.cli.jurl.common.AnnotationsUtils.extractNamedName;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.annotations.Named;

public class NamedTest {

    @Test
    public void instantiateNamed() {
        TestNamed named = new TestNamed();

        Assertions.assertEquals("test", extractNamedName(named));
        Assertions.assertEquals("", extractNamedAlias(named));
        Assertions.assertTrue(extractNamedAllowOverride(named));
    }

    @Named(name = "test", allowOverride = true)
    public static class TestNamed {

    }
}
