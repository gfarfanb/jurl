package com.legadi.jurl.executor.reader;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;

public interface OutputReader {

    boolean accepts(String contentType);

    boolean isPrintable();

    Map<String, String> apply(Path sourcePath, Set<String> params, String paramPrefix);

    default boolean accepts(String contentType, String... types) {
        return Arrays.stream(types)
            .anyMatch(type -> type.equalsIgnoreCase(contentType));
    }
}
