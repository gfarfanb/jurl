package com.legadi.jurl.executor.decoder;

import java.nio.file.Path;
import java.util.Arrays;

import com.legadi.jurl.common.Evaluable;

public interface OutputDecoder extends Evaluable {

    @Override
    default boolean accepts(String contentEncoding) {
        return Arrays.stream(types())
            .anyMatch(type -> type.equalsIgnoreCase(contentEncoding));
    }

    String[] types();

    Path apply(Path sourcePath);
}
