package com.legadi.cli.jurl.common;

import static java.util.logging.Level.FINE;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Command {

    private static final Logger LOGGER = Logger.getLogger(CommonUtils.class.getName());

    public static void exec(Settings settings, boolean inInterpreter, Level exitCodeLevel, String... commands) {
        exec(settings, out -> {}, inInterpreter, exitCodeLevel, commands);
    }

    public static void exec(Settings settings, Consumer<String> consumer,
            boolean inInterpreter, Level exitCodeLevel, String... commands) {
        StringExpander stringExpander = new StringExpander(settings);
        ProcessBuilder processBuilder = new ProcessBuilder();
        List<String> commandParts = new ArrayList<>();

        if(inInterpreter) {
            String os = System.getProperty("os.name").toLowerCase(Locale.ROOT);
            if(os.contains("win")) {
                commandParts.add("cmd.exe");
                commandParts.add("/c");
            } else {
                commandParts.add("/bin/bash");
                commandParts.add("-c");
            }
        }

        for(String command : commands) {
            commandParts.add(stringExpander.replaceAllInContent(command));
        }

        String[] cmd = commandParts.toArray(new String[commandParts.size()]);

        LOGGER.fine("Executing command: " + Arrays.toString(cmd));
        processBuilder.command(commandParts);

        try {
            Process process = processBuilder.start();

            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    consumer.accept(line);
                    LOGGER.fine("> " + line);
                }
            }

            LOGGER.log(exitCodeLevel, Arrays.toString(cmd) + ": exit " + process.waitFor());
        } catch (IOException | InterruptedException ex) {
            LOGGER.severe("Error on executing command "
                + Arrays.toString(cmd) + " - " + ex.getMessage());
            LOGGER.log(FINE, "Unable to execute command + "
                + Arrays.toString(cmd), ex);
        }
    }
}
