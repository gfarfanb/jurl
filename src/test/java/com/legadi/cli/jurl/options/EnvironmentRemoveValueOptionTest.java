package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.JsonUtils.writeJsonFile;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.SkipExecutionException;

public class EnvironmentRemoveValueOptionTest extends OptionTest<EnvironmentRemoveValueOption> {

    public EnvironmentRemoveValueOptionTest() {
        super("--env-rm");
    }

    @Test
    public void removeEnvironmentValueValidation() {
        Settings settings = new Settings();
        String env = UUID.randomUUID().toString();
        Path envPath = settings.getConfigPath().resolve("config." + env + ".json");

        Map<String, String> envProperties = new HashMap<>();
        String fieldValue = UUID.randomUUID().toString();

        envProperties.put(fieldValue, fieldValue);
        writeJsonFile(envPath, envProperties);

        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl(
                "-er", env, fieldValue
            ));

        envProperties = loadJsonFile(envPath.toString(), new TypeToken<Map<String, String>>() {});

        Assertions.assertFalse(envProperties.containsKey(fieldValue));
        Assertions.assertTrue(envPath.toFile().delete());
        Assertions.assertFalse(envPath.toFile().exists());
    }
}
