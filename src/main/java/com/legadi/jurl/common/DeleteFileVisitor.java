package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.trim;
import static java.util.logging.Level.FINE;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DeleteFileVisitor extends SimpleFileVisitor<Path> {

    private static final Logger LOGGER = Logger.getLogger(DeleteFileVisitor.class.getName());

    private final Pattern fileDatePattern = Pattern.compile("^.*(\\d{4}\\-\\d{2}\\-\\d{2}).*");
    private final DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE;
    private final LocalDate untilDateInclusive;

    public DeleteFileVisitor(LocalDate untilDateInclusive) {
        this.untilDateInclusive = untilDateInclusive;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        if(attrs.isSymbolicLink()) {
            return FileVisitResult.CONTINUE;
        }

        if(untilDateInclusive == null) {
            return silentDelete(file);
        }

        Matcher matcher = fileDatePattern.matcher(file.getFileName().toString());

        if(matcher.find()) {
            String extractedDate = trim(matcher.group(1));
            LocalDate fileDate = LocalDate.from(formatter.parse(extractedDate));

            if(fileDate.isBefore(untilDateInclusive) || fileDate.isEqual(untilDateInclusive)) {
                return silentDelete(file);
            } else {
                return FileVisitResult.CONTINUE;
            }
        } else {
            return FileVisitResult.CONTINUE;
        }
    }

    @Override
    public FileVisitResult postVisitDirectory(Path directory, IOException ex) throws IOException {
        if(!isDirectoryEmpty(directory)) {
            return FileVisitResult.CONTINUE;
        }

        return silentDelete(directory);
    }

    private boolean isDirectoryEmpty(Path directoryPath) {
        try (DirectoryStream<Path> directory = Files.newDirectoryStream(directoryPath)) {
            return !directory.iterator().hasNext();
        } catch(Exception ex) {
            LOGGER.log(FINE, "Unable to validate directory: " + directoryPath + " - " + ex.getMessage(), ex);
            return false;
        }
    }

    private FileVisitResult silentDelete(Path path) {
        try {
            Files.delete(path);
            LOGGER.info("Deleted: " + path);
        } catch(Exception ex) {
            LOGGER.log(FINE, "Unable to delete file: " + path + " - " + ex.getMessage(), ex);
        }
        return FileVisitResult.CONTINUE;
    }
}
