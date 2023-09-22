package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.createDirectories;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.strip;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

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
        List<String> pathParts = new ArrayList<>();

        if(isNotBlank(requestPath)) {
            pathParts.add(requestPath);
        }

        if(isNotBlank(requestName)) {
            pathParts.add(requestName);
        }

        if(isNotBlank(folder)) {
            pathParts.add(folder);
        }

        Path filePath = Paths.get(mainPath.toString(), pathParts.toArray(new String[pathParts.size()]));

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
