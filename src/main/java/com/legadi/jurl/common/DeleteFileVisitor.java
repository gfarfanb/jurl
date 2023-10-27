package com.legadi.jurl.common;

import static java.util.logging.Level.FINE;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.logging.Logger;

public class DeleteFileVisitor extends SimpleFileVisitor<Path> {

    private static final Logger LOGGER = Logger.getLogger(DeleteFileVisitor.class.getName());

    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
    private final LocalDate untilDateInclusive;

    public DeleteFileVisitor(LocalDate untilDateInclusive) {
        this.untilDateInclusive = untilDateInclusive;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        if(attrs.isSymbolicLink()) {
            return FileVisitResult.CONTINUE;
        }

        LocalDateTime lastModifiedTime = LocalDateTime.ofInstant(
            attrs.lastModifiedTime().toInstant(), ZoneOffset.UTC);

        if(untilDateInclusive == null) {
            return silentDelete(file, lastModifiedTime(lastModifiedTime));
        }

        LocalDate fileDate = lastModifiedTime.toLocalDate();

        if(fileDate.isBefore(untilDateInclusive) || fileDate.isEqual(untilDateInclusive)) {
            return silentDelete(file, lastModifiedTime(lastModifiedTime));
        } else {
            return FileVisitResult.CONTINUE;
        }
    }

    @Override
    public FileVisitResult postVisitDirectory(Path directory, IOException ex) throws IOException {
        if(!isDirectoryEmpty(directory)) {
            return FileVisitResult.CONTINUE;
        }

        return silentDelete(directory, "empty-directory");
    }

    private boolean isDirectoryEmpty(Path directoryPath) {
        try (DirectoryStream<Path> directory = Files.newDirectoryStream(directoryPath)) {
            return !directory.iterator().hasNext();
        } catch(Exception ex) {
            LOGGER.log(FINE, "Unable to validate directory: " + directoryPath + " - " + ex.getMessage(), ex);
            return false;
        }
    }

    private String lastModifiedTime(LocalDateTime lastModifiedTime) {
        return "lastModifiedTime: " + formatter.format(lastModifiedTime);
    }

    private FileVisitResult silentDelete(Path path, String tag) {
        try {
            Files.delete(path);
            LOGGER.info("Deleted"
                + (tag != null ? " [" + tag + "]" : "")
                + ": " + path);
        } catch(Exception ex) {
            LOGGER.log(FINE, "Unable to delete file: " + path + " - " + ex.getMessage(), ex);
        }
        return FileVisitResult.CONTINUE;
    }
}
