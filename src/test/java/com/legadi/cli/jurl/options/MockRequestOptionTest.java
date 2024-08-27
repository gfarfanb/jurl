package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MOCK_REQUEST;

import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class MockRequestOptionTest extends OptionTest<MockRequestOption> {

    public MockRequestOptionTest() {
        super("--mock");
    }

    @Test
    public void mockRequestValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-m",
                "-n", "obtain",
                "src/test/resources/mock-request.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, "settings");
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, "response");
        Map<String, String> responseContent = loadJsonFile(response.getResponsePath().toString(), new TypeToken<Map<String, String>>() {});

        Assertions.assertEquals(Boolean.TRUE.toString(), settings.get(PROP_MOCK_REQUEST));
        Assertions.assertTrue(responseContent.containsKey("mockResponseField"));
        Assertions.assertDoesNotThrow(() -> UUID.fromString(responseContent.get("mockResponseField")));
    }
}