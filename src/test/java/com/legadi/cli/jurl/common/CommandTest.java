package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.Command.exec;
import static com.legadi.cli.jurl.common.SettingsConstants.EXTERNAL_OS_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_OS_NAME;
import static java.util.logging.Level.INFO;

import java.util.Locale;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CommandTest {

    @Test
    public void echo() {
        Settings settings = new Settings();

        try {
            Assertions.assertDoesNotThrow(
                () -> exec(settings, true, INFO, "echo test"));

            String os = System.getProperty(EXTERNAL_OS_NAME).toLowerCase(Locale.ROOT);

            if(os.contains("win")) {
                Assertions.assertDoesNotThrow(
                    () -> exec(settings, true, INFO, "echo %USERPROFILE%"));
                Assertions.assertDoesNotThrow(
                    () -> exec(settings, false, INFO, "echo %USERPROFILE%"));
            } else {
                Assertions.assertDoesNotThrow(
                    () -> exec(settings, true, INFO, "echo $HOME"));
            }

            System.setProperty(JURL_OS_NAME, "win");
            Assertions.assertDoesNotThrow(
                () -> exec(settings, true, INFO, "echo test"));

            System.setProperty(JURL_OS_NAME, "linux");
            Assertions.assertDoesNotThrow(
                () -> exec(settings, true, INFO, "echo test"));
        } finally {
            System.setProperty(JURL_OS_NAME, System.getProperty(EXTERNAL_OS_NAME));
        }
    }
}
