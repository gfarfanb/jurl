package com.legadi.cli.jurl.common;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;
import com.google.gson.ToNumberPolicy;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.legadi.cli.jurl.exception.CommandException;

public class JsonUtils {

    private static final Logger LOGGER = Logger.getLogger(JsonUtils.class.getName());

    private JsonUtils() {}

    private static final Gson GSON = new GsonBuilder()
        .setObjectToNumberStrategy(ToNumberPolicy.BIG_DECIMAL)
        .setPrettyPrinting()
        .create();

    public static void writeJsonFile(Path filePath, Object content) {
        try(Writer writer = Files.newBufferedWriter(filePath)) {
            GSON.toJson(content, writer);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to write JSON file: " + filePath, ex);
        }
    }

    public static <T> T loadJsonFile(String filePath, TypeToken<T> type) {
        File jsonFile = new File(filePath);

        if(jsonFile.exists()) {
            try(Reader reader = Files.newBufferedReader(jsonFile.toPath())) {
                T input = GSON.fromJson(reader, type.getType());

                LOGGER.fine("Loaded JSON file: " + filePath);
                return input;
            } catch(Exception ex) {
                throw new IllegalStateException("Unable to read JSON file: " + filePath, ex);
            }
        } else {
            throw new CommandException("JSON file not found: " + filePath);
        }
    }

    public static Map<String, String> loadInternalJsonProperties(String internalFilePath) {
        try {
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            InputStream jsonInputStream = classLoader.getResource(internalFilePath).openStream();

            return readJsonProperties(Paths.get(internalFilePath), jsonInputStream);
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to obtain internal file: " + internalFilePath, ex);
        }
    }

    public static Map<String, String> loadJsonProperties(Path filePath) {
        File jsonFile = filePath.toFile();

        if(jsonFile.exists()) {
            try {
                return readJsonProperties(filePath, Files.newInputStream(filePath));
            } catch(Exception ex) {
                throw new IllegalStateException("Unable to obtain file: " + filePath, ex);
            }
        } else {
            LOGGER.fine("JSON properties file not found: " + filePath);
            return new HashMap<>();
        }
    }

    private static Map<String, String> readJsonProperties(Path jsonPath, InputStream jsonInputStream) {
        try(Reader reader = new InputStreamReader(jsonInputStream)) {
            Map<String, String> jsonProperties = GSON.fromJson(reader, new TypeToken<Map<String, String>>() {});

            LOGGER.fine("Loaded JSON properties file: " + jsonPath);

            return jsonProperties;
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to read JSON properties: " + jsonPath, ex);
        }
    }

    public static <T> T jsonToObject(String json, TypeToken<T> type) {
        try {
            return GSON.fromJson(json, type.getType());
        } catch(JsonParseException ex) {
            throw new IllegalStateException("Invalid JSON: " + json, ex);
        }
    }

    public static String toJsonString(Object value) {
        return GSON.toJson(value);
    }

    public static boolean isArrayFile(Path sourcePath) {
        try (JsonReader jsonReader = new JsonReader(Files.newBufferedReader(sourcePath))) {
            switch (jsonReader.peek()) {
                case BEGIN_OBJECT:
                    return false;
                case BEGIN_ARRAY:
                    return true;
                default:
                    throw new IllegalStateException("Malformed JSON file: " + sourcePath);
            }
        } catch (Exception ex) {
            throw new IllegalStateException("Unable to read JSON file: " + sourcePath, ex);
        }
    }
}
