package com.legadi.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class NamedTest {

    @Test
    public void instantiateNamed() {
        TestNamed named = new TestNamed();

        Assertions.assertEquals("test", named.name());
        Assertions.assertNull(named.alias());
        Assertions.assertTrue(named.allowOverride());
    }

    public static class TestNamed implements Named {

        @Override
        public String name() {
            return "test";
        }

        @Override
        public boolean allowOverride() {
            return true;
        }
    }
}
