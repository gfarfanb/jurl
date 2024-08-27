package com.legadi.cli.jurl.executor.decoder;

import java.nio.file.Path;
import java.util.Arrays;

import com.legadi.cli.jurl.common.Evaluable;

public interface OutputDecoder extends Evaluable {

    @Override
    default boolean accepts(String contentEncoding) {
        return Arrays.stream(types())
            .anyMatch(type -> type.equalsIgnoreCase(contentEncoding));
    }

    String[] types();

    Path apply(Path sourcePath);
}
