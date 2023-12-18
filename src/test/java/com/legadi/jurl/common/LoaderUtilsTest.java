package com.legadi.jurl.common;

import static com.legadi.jurl.common.LoaderUtils.instantiate;
import static com.legadi.jurl.common.LoaderUtils.loadAndCacheInternalLines;
import static com.legadi.jurl.common.LoaderUtils.typeOf;

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
    public void typeOfValidation() {
        Class<?> type = Assertions.assertDoesNotThrow(() -> typeOf("com.legadi.jurl.common.LoaderUtilsTest$TestLoader"));

        Assertions.assertEquals(TestLoader.class, type);
    }

    @Test
    public void typeOfNotFound() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> typeOf("com.legadi.jurl.common.ClassNotFound"));
    }

    @Test
    public void instantiateValidation() {
        Assertions.assertDoesNotThrow(() -> instantiate(TestLoader.class));

        Object[] args = null;
        Assertions.assertDoesNotThrow(() -> instantiate(TestLoader.class, args));
    }

    @Test
    public void instantiateFailed() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> instantiate(TestLoader.class, "A"));
    }

    public static class TestLoader {

    }

    public static class TestLoaderArgs {

        public TestLoaderArgs(String name) {}
    }
}
