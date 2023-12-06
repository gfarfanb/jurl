package com.legadi.jurl.common;

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

    public static class TestLoader {

    }

    public static class TestLoaderArgs {

        public TestLoaderArgs(String name) {}
    }
}
