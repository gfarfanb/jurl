package com.legadi.jurl.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

public class LoaderUtils {

    private static final Logger LOGGER = Logger.getLogger(LoaderUtils.class.getName());

    private static final Gson GSON = new Gson();
    private static Map<String, List<String>> CACHED_LINES = new HashMap<>();

    public static Map<String, String> loadInternalJsonProperties(String internalFilePath) {
        try {
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            InputStream jsonInputStream = classLoader.getResource(internalFilePath).openStream();

            return readJsonProperties(internalFilePath, jsonInputStream);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to obtain internal file: " + internalFilePath, ex);
        }
    }

    public static Map<String, String> loadJsonProperties(String filePath) {
        File jsonFile = new File(filePath);

        if(jsonFile.exists()) {
            try {
                return readJsonProperties(filePath, new FileInputStream(jsonFile));
            } catch(FileNotFoundException ex) {
                throw new IllegalStateException("Unable to obtain file: " + filePath, ex);
            }
        } else {
            LOGGER.fine("JSON properties file not found: " + filePath);
            return new HashMap<>();
        }
    }

    private static Map<String, String> readJsonProperties(String jsonPath, InputStream jsonInputStream) {
        try(Reader reader = new InputStreamReader(jsonInputStream)) {
            Map<String, String> jsonProperties = GSON.fromJson(reader, new TypeToken<Map<String, String>>() {});

            LOGGER.fine("Loaded JSON properties file: " + jsonPath);

            return jsonProperties;
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to read JSON properties: " + jsonPath, ex);
        }
    }

    public static Map<String, Credential> loadCredentials(String credentialsPath) {
        File credentialsFile = new File(credentialsPath);

        if(credentialsFile.exists()) {
            try(Reader reader = Files.newBufferedReader(credentialsFile.toPath())) {
                Map<String, Credential> credentials = GSON.<List<Credential>>fromJson(reader, new TypeToken<List<Credential>>() {}.getType())
                    .stream()
                    .collect(Collectors.toMap(
                        Credential::getId,
                        c -> c,
                        (c1, c2) -> { throw new CommandException("Credential ID already exists: " + c1.getId()); },
                        HashMap::new
                    ));

                LOGGER.fine("Loaded credentials file: " + credentialsPath);

                return credentials;
            } catch(IOException ex) {
                throw new IllegalStateException("Unable to read credentials file: " + credentialsPath, ex);
            }
        } else {
            LOGGER.fine("Credentials file not found: " + credentialsPath);
            return new HashMap<>();
        }
    }

    public static <T> T loadJsonFile(String filePath, TypeToken<T> type, boolean silent) {
        File jsonFile = new File(filePath);

        if(jsonFile.exists()) {
            try(Reader reader = Files.newBufferedReader(jsonFile.toPath())) {
                T input = GSON.fromJson(reader, type.getType());

                LOGGER.info("Loaded JSON file: " + filePath);
                return input;
            } catch(IOException ex) {
                throw new IllegalStateException("Unable to read JSON file: " + filePath, ex);
            }
        } else {
            if(silent) {
                LOGGER.fine("JSON file not found: " + filePath);
                return null;
            } else {
                throw new CommandException("JSON file not found: " + filePath);
            }
        }
    }

    public static <T> T jsonToObject(String json, TypeToken<T> type) {
        try {
            return GSON.fromJson(json, type.getType());
        } catch(JsonSyntaxException ex) {
            throw new IllegalStateException("Invalid JSON: " + json, ex);
        }
    }

    public static synchronized List<String> loadInternalLines(String internalFilePath) {
        if(CACHED_LINES.containsKey(internalFilePath)) {
            return new ArrayList<>(CACHED_LINES.get(internalFilePath));
        }

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

        try(InputStream inputStream = classLoader.getResource(internalFilePath).openStream();
                InputStreamReader inputReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
                BufferedReader bufferedReader = new BufferedReader(inputReader)) {
            List<String> lines = bufferedReader.lines().collect(Collectors.toList());

            CACHED_LINES.put(internalFilePath, lines);
            return new ArrayList<>(lines);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to obtain internal file: " + internalFilePath, ex);
        }
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
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to print file: " + filePath, ex);
        }
    }
}
