package com.legadi.cli.jurl.options;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.SkipExecutionException;

public class HelpOptionTest extends OptionTest<HelpOption> {

    public HelpOptionTest() {
        super("--help");
    }

    @Test
    public void printHelpValidation() {
        String currentOS = System.getProperty("os.name");

        System.setProperty("os.name", "win");

        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl("-h"));

        System.setProperty("os.name", "linux");

        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl("-h"));

        System.setProperty("os.name", currentOS);
    }
}
