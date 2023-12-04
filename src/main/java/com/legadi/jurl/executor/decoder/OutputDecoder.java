package com.legadi.jurl.executor.decoder;

import java.nio.file.Path;
import java.util.Arrays;

public interface OutputDecoder {

    String[] types();

    Path apply(Path sourcePath);

    default boolean accepts(String contentEncoding) {
        return Arrays.stream(types())
            .anyMatch(type -> type.equalsIgnoreCase(contentEncoding));
    }
}
