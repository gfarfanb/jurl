package com.legadi.cli.jurl.common;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

public class LoaderUtils {

    private static final Map<String, List<String>> CACHED_LINES = new HashMap<>();
    private static final Lock LOCK = new ReentrantLock();

    private LoaderUtils() {}

    public static List<String> loadAndCacheInternalLines(String internalFilePath) {
        LOCK.lock();

        try {

            if(CACHED_LINES.get(internalFilePath) != null) {
                return new ArrayList<>(CACHED_LINES.get(internalFilePath));
            }

            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

            try(InputStream inputStream = classLoader.getResource(internalFilePath).openStream();
                    InputStreamReader inputReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
                    BufferedReader bufferedReader = new BufferedReader(inputReader)) {
                List<String> lines = bufferedReader.lines().collect(Collectors.toList());

                CACHED_LINES.put(internalFilePath, lines);
                return new ArrayList<>(lines);
            } catch(Exception ex) {
                throw new IllegalStateException("Unable to obtain internal file: " + internalFilePath, ex);
            }

        } finally {
            LOCK.unlock();
        }
    }

    public static Class<?> typeOf(String typeClass) {
        try {
            return Class.forName(typeClass);
        } catch(ClassNotFoundException ex) {
            throw new IllegalStateException("Class not found for name: " + typeClass);
        }
    }

    @SuppressWarnings("unchecked")
    public static <T> T instantiate(Class<?> typeClass, Object... args) {
        try {
            Class<T> type = (Class<T>) typeClass;
            Class<?>[] argTypes = args == null || args.length == 0
                ? new Class<?>[0] : Arrays.stream(args).map(Object::getClass).toArray(Class[]::new);
            Constructor<T> constructor = type.getConstructor(argTypes);
            return constructor.newInstance(args);
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to instance from class: " + typeClass, ex);
        }
    }
}
