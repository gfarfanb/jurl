package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.isBlank;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.util.Map;
import java.util.function.Consumer;

public class WriterUtils {

    private WriterUtils() {}

    public static void writeLine(DataOutputStream dataOutputStream, String line, String charset) {
        try {
            if(isBlank(charset)) {
                charset = StandardCharsets.UTF_8.name();
            }
            byte[] input = line.getBytes(charset);
            dataOutputStream.write(input, 0, input.length);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write line [" + charset + "]: " + line , ex);
        }
    }

    public static void writeLine(BufferedWriter writer, String line) {
        try {
            writer.write(line);
            writer.newLine();
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write line: " + line, ex);
        }
    }

    public static void writeFile(Path filePath, String... lines) {
        try(BufferedWriter writer = Files.newBufferedWriter(filePath)) {
            int index = 0;
        
            for(String line : lines) {
                index++;

                writer.write(line);

                if(index < lines.length) {
                    writer.newLine();
                }
            }
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write file: " + filePath, ex);
        }
    }

    public static void appendToFile(File file, long seek, String... lines) {
        try(RandomAccessFile randomAccessFile = new RandomAccessFile(file, "rw")) {
            int index = 0;

            randomAccessFile.seek(seek);

            for(String line : lines) {
                index++;

                randomAccessFile.writeBytes(line);

                if(index < lines.length) {
                    randomAccessFile.writeBytes(System.getProperty("line.separator"));
                }
            }
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to append content to file: " + file, ex);
        }
    }

    public static void expandFile(Settings settings, Path inputFilePath, Path outputFilePath,
            Map<String, String> defaults) {
        expandFile(settings, inputFilePath, outputFilePath, defaults, line -> {});
    }

    public static void expandFile(Settings settings, Path inputFilePath, Path outputFilePath,
            Map<String, String> defaults, Consumer<String> lineConsumer) {
        StringExpander stringExpander = new StringExpander(settings);

        try(BufferedReader br = Files.newBufferedReader(inputFilePath);
                BufferedWriter writer = Files.newBufferedWriter(outputFilePath)) {
            br
                .lines()
                .map(stringExpander::replaceAllInContent)
                .forEach(line -> {
                    writeLine(writer, line);
                    lineConsumer.accept(line);
                });
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write expand file [" + inputFilePath + "]: " + outputFilePath, ex);
        }
    }

    public static Path createDirectories(Path path) {
        try {
            Files.createDirectories(path);
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to create directory: " + path, ex);
        }
        return path;
    }

    public static void printFile(Path filePath) {
        if(filePath == null) {
            return;
        }

        printFile(filePath.toString());
    }

    public static void printFile(String filePath) {
        if(filePath == null) {
            return;
        }

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                System.out.println(line);
            }
            System.out.println();
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to print file: " + filePath, ex);
        }
    }

    public static void cleanDirectory(Path directoryPath, SimpleFileVisitor<Path> visitor) {
        if(directoryPath == null) {
            return;
        }

        if(!Files.exists(directoryPath)) {
            return;
        }

        try {
            Files.walkFileTree(directoryPath, visitor);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to clean directory [" + visitor + "]: " + directoryPath, ex);
        }
    }

    public static boolean deleteFileFromPath(Path filePath) {
        try {
            filePath.toFile().delete();
            return true;
        } catch(Exception ex) {
            return false;
        }
    }
}
