package com.legadi.jurl.common;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.Writer;
import java.nio.file.Files;
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
}
