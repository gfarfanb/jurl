package com.legadi.jurl.options;

import java.nio.file.Path;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.SkipExecutionException;

public class EnvironmentCopyOptionTest extends OptionTest<EnvironmentCopyOption> {

    public EnvironmentCopyOptionTest() {
        super("--env-copy");
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
