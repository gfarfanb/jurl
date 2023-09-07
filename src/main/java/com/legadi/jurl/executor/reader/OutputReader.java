package com.legadi.jurl.executor.reader;

import java.nio.file.Path;
import java.util.Set;

public interface OutputReader<T> {

    T apply(Path sourcePath, Set<String> params, String paramPrefix);
}
