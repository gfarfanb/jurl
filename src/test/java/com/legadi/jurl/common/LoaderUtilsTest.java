package com.legadi.jurl.common;

import static com.legadi.jurl.common.LoaderUtils.instantiate;
import static com.legadi.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class LoaderUtilsTest {

    @Test
    public void loadAndCacheInternalLinesValidation() {
        List<String> lines = Assertions.assertDoesNotThrow(
            () -> loadAndCacheInternalLines("lines.txt"));

        Assertions.assertEquals(3, lines.size());

        List<String> cachedLines = Assertions.assertDoesNotThrow(
            () -> loadAndCacheInternalLines("lines.txt"));

        Assertions.assertEquals(3, cachedLines.size());
    }

    @Test
    public void loadAndCacheInternalLinesFileNotFound() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> loadAndCacheInternalLines("lines-file-not-found.txt"));
    }

    @Test
    public void instantiateValidation() {
        TestLoader instance = Assertions.assertDoesNotThrow(
            () -> instantiate(TestLoader.class.getName()));

        Assertions.assertNotNull(instance);
    }

    @Test
    public void instantiateConstructorWithArguments() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> instantiate(TestLoaderArgs.class.getName()));
    }

    public static class TestLoader {

    }

    public static class TestLoaderArgs {

        public TestLoaderArgs(String name) {}
    }
}
