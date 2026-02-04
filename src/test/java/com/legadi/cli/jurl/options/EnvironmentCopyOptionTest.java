package com.legadi.cli.jurl.options;

import java.nio.file.Path;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.SkipExecutionException;

public class EnvironmentCopyOptionTest extends OptionAbstractTest<EnvironmentCopyOption> {

    public EnvironmentCopyOptionTest() {
        super(EnvironmentCopyOption.class, false);
    }

    @Test
    public void copyEnvironmentValidation() {
        String env = UUID.randomUUID().toString();
        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl(
                "-ec", "default", env
            ));

        Settings settings = new Settings();
        Path generatedConfig = settings.getConfigPath().resolve("config." + env + ".json");

        Assertions.assertTrue(generatedConfig.toFile().exists());
        Assertions.assertTrue(generatedConfig.toFile().delete());
        Assertions.assertFalse(generatedConfig.toFile().exists());
    }
}
