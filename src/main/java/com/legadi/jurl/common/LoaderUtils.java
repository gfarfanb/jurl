package com.legadi.jurl.common;

import static com.legadi.jurl.common.JsonUtils.loadJsonFile;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.Credential;

public class LoaderUtils {

    private static final Logger LOGGER = Logger.getLogger(LoaderUtils.class.getName());

    private static final Map<String, List<String>> CACHED_LINES = new HashMap<>();

    private LoaderUtils() {}

    public static Map<String, Credential> loadCredentials(Path credentialsPath) {
        File credentialsFile = credentialsPath.toFile();

        if(credentialsFile.exists()) {
            Map<String, Credential> credentials = loadJsonFile(credentialsFile.toString(), new TypeToken<List<Credential>>() {})
                .stream()
                .collect(Collectors.toMap(
                    Credential::getId,
                    c -> c,
                    (c1, c2) -> { throw new CommandException("Credential ID already exists: " + c1.getId()); },
                    HashMap::new
                ));

            LOGGER.fine("Loaded credentials file: " + credentialsPath);

            return credentials;
        } else {
            LOGGER.fine("Credentials file not found: " + credentialsPath);
            return new HashMap<>();
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

    @SuppressWarnings("unchecked")
    public static <T> T instantiate(String typeClass) {
        try {
            Class<T> type = (Class<T>) Class.forName(typeClass);
            Constructor<T> constructor = type.getConstructor();
            return constructor.newInstance();
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to instance from: " + typeClass, ex);
        }
    }
}
