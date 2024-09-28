package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.WriterUtils.writeFile;

import java.nio.file.Path;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.SkipExecutionException;

public class EnvironmentSetValueOptionTest extends OptionAbstractTest<EnvironmentSetValueOption> {

    public EnvironmentSetValueOptionTest() {
        super("--env-set");
    }

    @Test
    public void setEnvironmentValueValidation() {
        Settings settings = new Settings();
        String env = UUID.randomUUID().toString();
        Path envPath = settings.getConfigPath().resolve("config." + env + ".json");

        writeFile(envPath, "{}");

        Assertions.assertThrows(SkipExecutionException.class,
            () -> jurl(
                "-es", env, "name", UUID.randomUUID().toString()
            ));

        Map<String, String> envProperties = loadJsonFile(envPath.toString(), new TypeToken<Map<String, String>>() {});

        Assertions.assertTrue(envProperties.containsKey("name"));
        Assertions.assertDoesNotThrow(() -> UUID.fromString(envProperties.get("name")));
        Assertions.assertTrue(envPath.toFile().delete());
        Assertions.assertFalse(envPath.toFile().exists());
    }
}
