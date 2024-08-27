package com.legadi.cli.jurl.embedded.wrong;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.time.LocalDate;

import com.legadi.cli.jurl.common.DeleteFileVisitor;

public class FailedDeleteFileVisitor extends DeleteFileVisitor {

    public FailedDeleteFileVisitor(LocalDate untilDateInclusive) {
        super(untilDateInclusive);
    }

    @Override
    public FileVisitResult postVisitDirectory(Path directory, IOException ex) throws IOException {
        throw new IOException("Trying postVisitDirectory(" + directory + ")");
    }
}
