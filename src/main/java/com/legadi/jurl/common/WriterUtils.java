package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.strip;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class WriterUtils {

    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();

    private WriterUtils() {}

    public static void writeJsonFile(String filename, Object content) {
        try(Writer writer = Files.newBufferedWriter(Paths.get(filename))) {
            GSON.toJson(content, writer);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write JSON file: " + filename, ex);
        }
    }

    public static void writeFile(String filename, String... lines) {
        try(BufferedWriter writer = Files.newBufferedWriter(Paths.get(filename))) {
            int index = 0;
        
            for(String line : lines) {
                index++;

                writer.write(line);

                if(index < lines.length) {
                    writer.newLine();
                }
            }
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write file: " + filename, ex);
        }
    }

    public static void appendToFile(File file, long seek, String... lines) {
        try(RandomAccessFile randomAccessFile = new RandomAccessFile(file, "rw")) {
            int index = 0;

            randomAccessFile.seek(seek);

            for(String line : lines) {
                index++;

                randomAccessFile.writeChars(line);

                if(index < lines.length) {
                    randomAccessFile.writeBytes(System.getProperty("line.separator"));
                }
            }
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to append content to file: " + file, ex);
        }
    }

    public static Path buildOutputFilePath(Settings settings, String path, String folder, String extension) {
        return buildFilePath(settings.getOutputPath(), path, folder, settings.getExecutionTag(), extension);
    }

    public static Path buildTemporalFilePath(Settings settings, String path, String folder, String extension) {
        return buildFilePath(settings.getTemporalPath(), path, folder, settings.getExecutionTag(), extension);
    }

    public static Path buildHistoryFilePath(Settings settings, String path, String filename, String extension) {
        return buildFilePath(settings.getHistoryPath(), path, null, filename, extension);
    }

    private static Path buildFilePath(Path mainPath, String path, String folder, String filename, String extension) {
        String[] pathParts = strip(path, File.separator).split(File.separator);
        StringBuilder filePathBuilder = new StringBuilder()
            .append(pathParts[pathParts.length - 1].replaceAll("\\.", "_"));
        
        if(isNotBlank(folder)) {
            filePathBuilder.append(File.separator).append(folder);
        }

        filePathBuilder
            .append(File.separator)
            .append(filename)
            .append('.')
            .append(extension);

        Path filePath = mainPath.resolve(filePathBuilder.toString());

        try {
            Files.createDirectories(filePath);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to create directory: " + filePath, ex);
        }

        return filePath;
    }

}
