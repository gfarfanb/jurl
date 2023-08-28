package com.legadi.jurl.common;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.Files;
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

public class Loader {

    private static final Logger LOGGER = Logger.getLogger(Loader.class.getName());

    private static final Gson GSON = new Gson();

    public static Map<String, String> loadInternalJsonProperties(String internalFilePath, boolean silent) {
        try {
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            InputStream jsonInputStream = classLoader.getResource(internalFilePath).openStream();

            return readJsonProperties(internalFilePath, jsonInputStream, silent);
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to obtain internal file: " + internalFilePath, ex);
        }
    }

    public static Map<String, String> loadJsonProperties(String filePath, boolean silent) {
        File jsonFile = new File(filePath);

        if(jsonFile.exists()) {
            try {
                return readJsonProperties(filePath, new FileInputStream(jsonFile), silent);
            } catch(FileNotFoundException ex) {
                throw new IllegalStateException("Unable to optain file: " + filePath, ex);
            }
        } else {
            if(silent) {
                LOGGER.fine("JSON properties file not found: " + filePath);
            } else {
                LOGGER.warning("JSON properties file not found: " + filePath);
            }
            return new HashMap<>();
        }
    }

    private static Map<String, String> readJsonProperties(String jsonPath, InputStream jsonInputStream, boolean silent) {
        try(Reader reader = new InputStreamReader(jsonInputStream)) {
            Map<String, String> jsonProperties = GSON.fromJson(reader, new TypeToken<Map<String, String>>() {});

            if(silent) {
                LOGGER.fine("Loaded JSON properties file: " + jsonPath);
            } else {
                LOGGER.info("Loaded JSON properties file: " + jsonPath);
            }

            return jsonProperties;
        } catch(IOException ex) {
            throw new IllegalStateException("Unable to read JSON properties: " + jsonPath, ex);
        }
    }

    public static Map<String, Credential> loadCredentials(String credentialsPath, boolean silent) {
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

                if(silent) {
                    LOGGER.fine("Loaded credentials file: " + credentialsPath);
                } else {
                    LOGGER.info("Loaded credentials file: " + credentialsPath);
                }

                return credentials;
            } catch(IOException ex) {
                throw new IllegalStateException("Unable to read credentials file: " + credentialsPath, ex);
            }
        } else {
            if(silent) {
                LOGGER.fine("Credentials file not found: " + credentialsPath);
            } else {
                LOGGER.warning("Credentials file not found: " + credentialsPath);
            }
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
}
