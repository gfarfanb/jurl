package com.legadi.jurl.common;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class OutputPathBuilderTest {

    @Test
    public void buildCommandPathValidation() {
        Settings settings = new Settings();
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath("src/test/resources/output-command-builder")
            .setRequestName("command-path")
            .setExtension("body");
        Path expected = Paths.get("./executions/src/test/resources/output-command-builder/command-path/"
            + settings.getTimestamp().toLocalDate() + "/"
            + settings.getExecutionTag() + ".body");

        Assertions.assertEquals(expected, pathBuilder.buildCommandPath());
    }

    @Test
    public void buildCommandPathNoRequestArguments() {
        Settings settings = new Settings();
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setExtension("body");
        Path expected = Paths.get("./executions/"
            + settings.getTimestamp().toLocalDate() + "/"
            + settings.getExecutionTag() + ".body");

        Assertions.assertEquals(expected, pathBuilder.buildCommandPath());
    }

    @Test
    public void buildHistoryPathValidation() {
        Settings settings = new Settings();
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath("src/test/resources/output-command-builder")
            .setRequestName("history-path")
            .setFilename(settings.getTimestamp().toLocalDate().toString())
            .setExtension("history.json");
        Path expected = Paths.get("./history/src/test/resources/output-command-builder/history-path/"
            + settings.getTimestamp().toLocalDate() + "/"
            + settings.getTimestamp().toLocalDate() + ".history.json");

        Assertions.assertEquals(expected, pathBuilder.buildHistoryPath());
    }

    @Test
    public void buildHistoryPathNoRequestArguments() {
        Settings settings = new Settings();
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setFilename(settings.getTimestamp().toLocalDate().toString())
            .setExtension("history.json");
        Path expected = Paths.get("./history/"
            + settings.getTimestamp().toLocalDate() + "/"
            + settings.getTimestamp().toLocalDate() + ".history.json");

        Assertions.assertEquals(expected, pathBuilder.buildHistoryPath());
    }
}
