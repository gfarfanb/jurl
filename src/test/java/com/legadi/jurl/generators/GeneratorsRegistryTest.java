package com.legadi.jurl.generators;

import static com.legadi.jurl.generators.GeneratorsRegistry.getValueByParam;
import static com.legadi.jurl.generators.GeneratorsRegistry.registerGenerator;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;

public class GeneratorsRegistryTest {

    @Test
    public void registerGeneratorCustom() {
        registerGenerator(TestGenerator.class.getName());

        String value = Assertions.assertDoesNotThrow(
            () -> getValueByParam(null, "test"));

        Assertions.assertEquals("test", value);
    }

    @Test
    public void notFound() {
        String value = Assertions.assertDoesNotThrow(
            () -> getValueByParam(null, "not-found"));

        Assertions.assertNull(value);
    }

    public static class TestGenerator implements Generator {

        @Override
        public String tag() {
            return "test";
        }

        @Override
        public String getValue(Settings settings, String param) {
            return "test";
        }
    }
}
