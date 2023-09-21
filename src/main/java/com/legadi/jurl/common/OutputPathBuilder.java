package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.strip;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class OutputPathBuilder {

    private final Settings settings;

    private String requestPath;
    private String requestName;
    private String filename;
    private String extension;

    public OutputPathBuilder(Settings settings) {
        this.settings = settings;
    }

    public OutputPathBuilder setRequestPath(String requestPath) {
        String[] pathParts = strip(requestPath, File.separator + ".").split(File.separator);
        this.requestPath = pathParts[pathParts.length - 1].replaceAll("\\.", "_");
        return this;
    }

    public OutputPathBuilder setRequestName(String requestName) {
        this.requestName = requestName;
        return this;
    }

    public OutputPathBuilder setFilename(String filename) {
        this.filename = filename;
        return this;
    }

    public OutputPathBuilder setLocalDateFilename() {
        this.filename = settings.getTimestamp().toLocalDate().toString();
        return this;
    }

    public OutputPathBuilder setExtension(String extension) {
        this.extension = extension;
        return this;
    }

    public Path buildOutputPath() {
        return buildFilePath(settings.getOutputPath(), null);
    }

    public Path buildTemporalPath() {
        return buildFilePath(settings.getTemporalPath(), null);
    }

    public Path buildHistoryPath() {
        return buildFilePath(settings.getHistoryPath(), settings.getTimestamp().toLocalDate().toString());
    }

    private Path buildFilePath(Path mainPath, String folder) {
        StringBuilder filePathBuilder = new StringBuilder();

        if(isNotBlank(requestPath)) {
            filePathBuilder.append(File.separator).append(requestPath);
        }

        if(isNotBlank(requestName)) {
            filePathBuilder.append(File.separator).append(requestName);
        }

        if(isNotBlank(folder)) {
            filePathBuilder.append(File.separator).append(folder);
        }

        if(isNotBlank(filename)) {
            filePathBuilder.append(File.separator).append(filename);
        } else {
            filePathBuilder.append(File.separator).append(settings.getExecutionTag());
        }

        if(isNotBlank(extension)) {
            filePathBuilder.append('.').append(extension);
        }

        Path filePath = mainPath.resolve(filePathBuilder.toString());

        try {
            Files.createDirectories(filePath);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create directory: " + filePath, ex);
        }

        return filePath;
    }
}
