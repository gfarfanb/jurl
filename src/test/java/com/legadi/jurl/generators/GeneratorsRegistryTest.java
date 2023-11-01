package com.legadi.jurl.generators;

import static com.legadi.jurl.generators.GeneratorsRegistry.findGeneratorByName;
import static com.legadi.jurl.generators.GeneratorsRegistry.registerGenerator;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class GeneratorsRegistryTest {

    @Test
    public void registerGeneratorCustom() {
        registerGenerator(TestGenerator.class.getName());

        Generator generator = Assertions.assertDoesNotThrow(
            () -> findGeneratorByName("test"));

        Assertions.assertNotNull(generator);
        Assertions.assertEquals("test-param", generator.getValue(null, "test-param"));
    }

    @Test
    public void duplicate() {
        Assertions.assertThrows(CommandException.class,
            () -> registerGenerator(DecimalGenerator::new));
    }

    @Test
    public void notFound() {
        Generator generator = Assertions.assertDoesNotThrow(
            () -> findGeneratorByName("not-found"));

        Assertions.assertNull(generator);
    }

    public static class TestGenerator implements Generator {

        @Override
        public String name() {
            return "test";
        }

        @Override
        public String getValue(Settings settings, String param) {
            return param;
        }
    }
}
