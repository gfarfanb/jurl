package com.legadi.jurl.executor.reader;

import static com.legadi.jurl.executor.reader.OutputReaderRegistry.findByContentType;
import static com.legadi.jurl.executor.reader.OutputReaderRegistry.registerReader;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class OutputReaderRegistryTest {

    @Test
    public void registerReaderCustom() {
        registerReader(TestReader.class.getName());

        Optional<OutputReader> reader = Assertions.assertDoesNotThrow(
            () -> findByContentType("class/test"));

        Assertions.assertTrue(reader.isPresent());
        Assertions.assertTrue(reader.get() instanceof TestReader);
    }

    @Test
    public void notFound() {
        Optional<OutputReader> reader = Assertions.assertDoesNotThrow(
            () -> findByContentType("not/found"));

        Assertions.assertFalse(reader.isPresent());
    }

    public static class TestReader implements OutputReader {

        @Override
        public String[] types() {
            return new String[] { "class/test" };
        }

        @Override
        public boolean isPrintable() {
            return true;
        }

        @Override
        public Map<String, String> apply(Path sourcePath, Path outputPath, Set<String> params, String paramPrefix) {
            return new HashMap<>();
        }
    }
}
