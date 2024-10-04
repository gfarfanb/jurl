package com.legadi.cli.jurl.options;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.exception.SkipExecutionException;

public class HelpOptionTest extends OptionAbstractTest<HelpOption> {

    public HelpOptionTest() {
        super("--help");
    }

    @Test
    public void printHelpValidation() {
        try {
            System.setProperty("jurl.os.name", "win");

            Assertions.assertThrows(SkipExecutionException.class,
                () -> jurl("-h"));

            System.setProperty("jurl.os.name", "linux");

            Assertions.assertThrows(SkipExecutionException.class,
                () -> jurl("-h"));
        } finally {
            System.setProperty("jurl.os.name", System.getProperty("os.name"));
        }
    }
}
