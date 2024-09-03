package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.Command.exec;
import static java.util.logging.Level.INFO;

import java.util.Locale;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CommandTest {

    @Test
    public void echo() {
        Settings settings = new Settings();
        String os = System.getProperty("os.name").toLowerCase(Locale.ROOT);

        if(os.contains("win")) {
            Assertions.assertDoesNotThrow(
                () -> exec(settings, true, INFO, "echo %USERPROFILE%"));
            Assertions.assertDoesNotThrow(
                () -> exec(settings, false, INFO, "echo %USERPROFILE%"));
        } else {
            Assertions.assertDoesNotThrow(
                () -> exec(settings, true, INFO, "echo $HOME"));
            Assertions.assertDoesNotThrow(
                () -> exec(settings, false, INFO, "echo $HOME"));
        }
    }
}
