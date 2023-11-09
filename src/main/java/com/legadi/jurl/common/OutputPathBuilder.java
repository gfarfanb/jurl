package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.fileSeparatorAsDelimiter;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.stripStart;
import static com.legadi.jurl.common.WriterUtils.createDirectories;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class OutputPathBuilder {

    private final Settings settings;

    private String[] requestPath;
    private String requestName;
    private String filename;
    private String extension;

    public OutputPathBuilder(Settings settings) {
        this.settings = settings;
    }

    public OutputPathBuilder setRequestPath(String requestPath) {
        String[] pathParts = stripStart(requestPath, File.separator + ".").split(fileSeparatorAsDelimiter());
        this.requestPath = Arrays
            .stream(pathParts)
            .map(part -> part.replaceAll(" ", "_"))
            .map(part -> part.replaceAll("\\.", "_"))
            .toArray(String[]::new);
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

    public OutputPathBuilder setExtension(String extension) {
        this.extension = extension;
        return this;
    }

    public Path buildCommandPath() {
        return buildFilePath(settings.getExecutionPath(), settings.getTimestamp().toLocalDate().toString());
    }

    public Path buildHistoryPath() {
        return buildFilePath(settings.getHistoryPath(), settings.getTimestamp().toLocalDate().toString());
    }

    private Path buildFilePath(Path basePath, String folder) {
        List<String> pathParts = new ArrayList<>();

        if(isNotEmpty(requestPath)) {
            Arrays.stream(requestPath).forEach(pathParts::add);
        }

        if(isNotBlank(requestName)) {
            pathParts.add(requestName);
        }

        if(isNotBlank(folder)) {
            pathParts.add(folder);
        }

        Path filePath = Paths.get(basePath.toString(), pathParts.toArray(new String[pathParts.size()]));

        return createDirectories(filePath).resolve(getFilename());
    }

    private String getFilename() {
        String ext = isNotBlank(extension) ? "." + extension : "";

        if(isNotBlank(filename)) {
            return filename + ext;
        } else {
            return settings.getExecutionTag() + ext;
        }
    }
}
