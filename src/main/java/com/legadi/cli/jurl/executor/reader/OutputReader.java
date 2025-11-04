package com.legadi.cli.jurl.executor.reader;

import java.nio.file.Path;
import java.util.Map;
import java.util.Set;

public interface OutputReader {

    boolean isPrintable();

    Map<String, String> apply(Path sourcePath, Path outputPath, Set<String> params, String paramPrefix);
}
