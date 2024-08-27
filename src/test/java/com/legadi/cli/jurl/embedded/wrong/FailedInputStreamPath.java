package com.legadi.cli.jurl.embedded.wrong;

import java.nio.file.FileSystem;
import java.nio.file.Path;

public class FailedInputStreamPath extends FailedFileSystemPath {

    public FailedInputStreamPath(Path path) {
        super(path);
    }

    @Override
    public FileSystem getFileSystem() {
        return new UnreadableFileSystem(path.getFileSystem());
    }
}
