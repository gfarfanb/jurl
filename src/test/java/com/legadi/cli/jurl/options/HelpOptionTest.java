package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.SettingsConstants.EXTERNAL_OS_NAME;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_OS_NAME;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.SkipExecutionException;

public class HelpOptionTest extends OptionAbstractTest<HelpOption> {

    public HelpOptionTest() {
        super("--help", false);
    }

    @Test
    public void printHelpValidation() {
        try {
            System.setProperty(JURL_OS_NAME, "win");

            Assertions.assertThrows(SkipExecutionException.class,
                () -> jurl("-h"));

            System.setProperty(JURL_OS_NAME, "linux");

            Assertions.assertThrows(SkipExecutionException.class,
                () -> jurl("-h"));
        } finally {
            System.setProperty(JURL_OS_NAME, System.getProperty(EXTERNAL_OS_NAME));
        }
    }
}
