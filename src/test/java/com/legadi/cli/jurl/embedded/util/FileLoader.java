package com.legadi.cli.jurl.embedded.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

public class FileLoader {

    private FileLoader() {}

    public static List<String> readLines(Path filePath) throws IOException {
        try(InputStream inputStream = Files.newInputStream(filePath);
                InputStreamReader inputReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
                BufferedReader bufferedReader = new BufferedReader(inputReader)) {
            return bufferedReader.lines().collect(Collectors.toList());
        }
    }
}
