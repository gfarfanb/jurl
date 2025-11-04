package com.legadi.cli.jurl.executor.decoder;

import java.nio.file.Path;

public interface OutputDecoder {

    Path apply(Path sourcePath);
}
