package com.legadi.cli.jurl.options;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.SettingsConstants.PROP_MERGE_BODY_USING_TYPE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.RESPONSE;
import static com.legadi.cli.jurl.embedded.util.ObjectName.SETTINGS;
import static com.legadi.cli.jurl.executor.http.HTTPRequestModifier.BODY_TEMPORAL_PATH;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.model.http.HTTPResponseEntry;

public class MergeBodyOptionTest extends OptionAbstractTest<MergeBodyOption> {

    public MergeBodyOptionTest() {
        super(MergeBodyOption.class, false);
    }

    @Test
    public void mergeBodyValidation() {
        UUID correlationId = Assertions.assertDoesNotThrow(
            () -> jurl(
                "-mb", "json",
                "-ns",
                "-s", "basic.functions.id", UUID.randomUUID().toString(),
                "-s", "basic.functions.entity", "src/test/resources/basic-functions.body.json",
                "-n", "update",
                "src/test/resources/basic-functions.spec.http"
            ));

        Settings settings = requestCatcher.getLast(correlationId, SETTINGS);
        HTTPResponseEntry response = requestCatcher.getLast(correlationId, RESPONSE);

        Assertions.assertEquals("json", settings.get(PROP_MERGE_BODY_USING_TYPE));
        Assertions.assertEquals(404, response.getStatusCode());

        Map<String, Object> body = loadJsonFile(settings.get(BODY_TEMPORAL_PATH), new TypeToken<Map<String, Object>>() {}, null);

        Assertions.assertEquals("187b03f1-55d5-4d6b-954d-51cf5f20c952", body.get("access"));
        Assertions.assertEquals("Basic Functions", body.get("name"));
        Assertions.assertEquals("jurl@test.com", body.get("email"));
        Assertions.assertEquals("bfunctions", body.get("nickname"));
        Assertions.assertEquals(BigDecimal.valueOf(5.5d), body.get("amount"));
        Assertions.assertTrue((Boolean) body.get("active"));
        Assertions.assertEquals(BigDecimal.valueOf(55), body.get("coins"));
        Assertions.assertTrue(body.get("bio").toString().startsWith("Platonem habeo interdum"));
        Assertions.assertEquals("A", body.get("type"));
        Assertions.assertEquals("2023-10-13T03:50:54.792", body.get("timestamp"));
    }
}
