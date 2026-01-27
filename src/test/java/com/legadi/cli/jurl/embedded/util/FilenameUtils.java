package com.legadi.cli.jurl.embedded.util;

import java.io.File;
import java.util.Arrays;
import java.util.stream.Collectors;

public class FilenameUtils {

    private FilenameUtils() {}

    public static String toSystemSeparator(String path) {
        if(path == null) {
            return null;
        }
        return Arrays.stream(path.split("\\/|\\\\"))
            .collect(Collectors.joining(File.separator));
    }
}
