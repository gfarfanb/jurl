package com.legadi.jurl.executor.reader;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;

public interface OutputReader {

    String[] types();

    boolean isPrintable();

    Map<String, String> apply(Path sourcePath, Path outputPath, Set<String> params, String paramPrefix);

    default boolean accepts(String contentType) {
        return Arrays.stream(types())
            .anyMatch(type -> type.equalsIgnoreCase(contentType));
    }
}
