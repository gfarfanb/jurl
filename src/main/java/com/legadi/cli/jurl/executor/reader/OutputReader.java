package com.legadi.cli.jurl.executor.reader;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import com.legadi.cli.jurl.common.Evaluable;

public interface OutputReader extends Evaluable {

    @Override
    default boolean accepts(String contentType) {
        return Arrays.stream(types())
            .anyMatch(type -> contentType.toLowerCase().startsWith(type.toLowerCase()));
    }

    String[] types();

    boolean isPrintable();

    Map<String, String> apply(Path sourcePath, Path outputPath, Set<String> params, String paramPrefix);
}
