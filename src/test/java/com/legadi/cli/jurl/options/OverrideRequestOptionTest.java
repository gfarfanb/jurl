package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_OVERRIDE_REQUEST_FILE_PATH;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPRequestEntry;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class OverrideRequestOptionTest extends OptionTest<OverrideRequestOption> {

    public OverrideRequestOptionTest() {
        super("--override-request");
    }

    @Test
    public void overrideRequestValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-or", "src/test/resources/basic-functions.body.json",
                "-n", "create",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, "settings");
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, "response");

        Assertions.assertEquals("src/test/resources/basic-functions.body.json", settings.get(PROP_OVERRIDE_REQUEST_FILE_PATH));
        Assertions.assertEquals(201, response.getStatusCode());

        HTTPRequestEntry request = requestCatcher.getLast(correlationId, "request");
        Map<String, Object> body = jsonToObject(request.getBodyContent(), new TypeToken<Map<String, Object>>() {});

        Assertions.assertEquals("187b03f1-55d5-4d6b-954d-51cf5f20c952", body.get("access"));
        Assertions.assertEquals("Basic Functions", body.get("name"));
        Assertions.assertEquals("basic.functions@test.com", body.get("email"));
        Assertions.assertEquals("bfunctions", body.get("nickname"));
        Assertions.assertEquals(BigDecimal.valueOf(5.5d), body.get("amount"));
        Assertions.assertTrue((Boolean) body.get("active"));
        Assertions.assertEquals(BigDecimal.valueOf(55), body.get("coins"));
        Assertions.assertTrue(body.get("bio").toString().startsWith("Platonem habeo interdum"));
        Assertions.assertEquals("A", body.get("type"));
        Assertions.assertEquals("2023-10-13T03:50:54.792", body.get("timestamp"));
    }
}
