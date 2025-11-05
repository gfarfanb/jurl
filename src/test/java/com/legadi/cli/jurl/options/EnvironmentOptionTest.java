package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.writeJsonFile;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class EnvironmentOptionTest extends OptionAbstractTest<EnvironmentOption> {

    public EnvironmentOptionTest() {
        super(EnvironmentOption.class, false);
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

        Settings envSettings = requestCatcher.getLast(correlationId, SETTINGS);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertEquals(env, envSettings.getEnvironment());
        Assertions.assertEquals(env, envSettings.get("env"));
        Assertions.assertEquals(201, response.getStatusCode());

        Assertions.assertTrue(envPath.toFile().delete());
        Assertions.assertFalse(envPath.toFile().exists());
        Assertions.assertTrue(overridePath.toFile().delete());
        Assertions.assertFalse(overridePath.toFile().exists());
    }
}
