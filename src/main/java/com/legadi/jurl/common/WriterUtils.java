package com.legadi.jurl.common;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class WriterUtils {

    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();

    private WriterUtils() {}

    public static void writeJsonFile(String fileName, Object content) {
        try(Writer writer = Files.newBufferedWriter(Paths.get(fileName))) {
            GSON.toJson(content, writer);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write JSON file: " + fileName, ex);
        }
    }
}
