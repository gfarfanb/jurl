package com.legadi.jurl.options;

import static com.legadi.jurl.common.JsonUtils.writeJsonFile;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.model.http.HTTPResponseEntry;

public class EnvironmentOptionTest extends OptionTest<EnvironmentOption> {

    public EnvironmentOptionTest() {
        super("--env");
    }

    @Test
    public void setEnvironmetValidation() {
        Settings settings = new Settings();
        String env = UUID.randomUUID().toString();
        Path envPath = settings.getConfigPath().resolve("config." + env + ".json");
        Path overridePath = settings.getConfigOutputPath().resolve("override." + env + ".json");
        Map<String, String> envProperties = new HashMap<>();

        envProperties.put("env", env);
        envProperties.put("local.server.port", Integer.toString(port));
        envProperties.put("request.catcher.identifier", requestCatcherId);

        writeJsonFile(envPath, envProperties);

        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-e", env,
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings envSettings = requestCatcher.get(correlationId, "settings");
        HTTPResponseEntry response = requestCatcher.get(correlationId, "response");

        Assertions.assertEquals(env, envSettings.getEnvironment());
        Assertions.assertEquals(env, envSettings.get("env"));
        Assertions.assertEquals(201, response.getStatusCode());

        Assertions.assertTrue(envPath.toFile().delete());
        Assertions.assertFalse(envPath.toFile().exists());
        Assertions.assertTrue(overridePath.toFile().delete());
        Assertions.assertFalse(overridePath.toFile().exists());
    }
}
