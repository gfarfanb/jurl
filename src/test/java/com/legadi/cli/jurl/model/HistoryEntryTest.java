package com.legadi.cli.jurl.model;

import static com.legadi.cli.jurl.common.SettingsConstants.TAG_FORMATTER;
import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HistoryEntryTest {

    @Test
    public void setterGetterValidation() {
        HistoryEntry model = new HistoryEntry();
        UUID uuid = UUID.randomUUID();
        LocalDateTime now = LocalDateTime.now();
        String curl = "curl -X POST"
            + " -H \"Request-Catcher: " + uuid + "\""
            + " -H \"Content-Type: application/json\""
            + " --data-binary \"@./executions/src/test/resources/api-definition.http/request-name/"
            + ISO_LOCAL_DATE.format(now) + "/" + TAG_FORMATTER.format(now) + ".body\""
            + " \"http://localhost:42121/basic/body\"";
        String bodyPath = "./executions/src/test/resources/api-definition.http/request-name/"
            + ISO_LOCAL_DATE.format(now) + "/" + TAG_FORMATTER.format(now) + ".body";

        model.setCurl(curl);
        model.setResult("HTTP/1.1 201 OK");
        model.setRequestPath("src/test/resources/api-definition.http");
        model.setRequestName("request-name");
        model.setEnvironment("default");
        model.setWorkspacePath("/workspaces/jurl");
        model.setTimestamp(now.toInstant(ZoneOffset.UTC).toEpochMilli());
        model.setExecutionTag(TAG_FORMATTER.format(now));
        model.setNanoTime(2050708L);
        model.setAssertions(2);
        model.setFailures(0);
        model.setIndex("1/1");

        Map<String, Object> details = new HashMap<>();
        details.put("bodyPath", bodyPath);
        model.setDetails(details);

        Assertions.assertEquals(curl, model.getCurl());
        Assertions.assertEquals("HTTP/1.1 201 OK", model.getResult());
        Assertions.assertEquals("src/test/resources/api-definition.http", model.getRequestPath());
        Assertions.assertEquals("request-name", model.getRequestName());
        Assertions.assertEquals("default", model.getEnvironment());
        Assertions.assertEquals("/workspaces/jurl", model.getWorkspacePath());
        Assertions.assertEquals(now.toInstant(ZoneOffset.UTC).toEpochMilli(), model.getTimestamp());
        Assertions.assertEquals(TAG_FORMATTER.format(now), model.getExecutionTag());
        Assertions.assertEquals(2050708L, model.getNanoTime());
        Assertions.assertEquals(2, model.getAssertions());
        Assertions.assertEquals(0, model.getFailures());
        Assertions.assertEquals("1/1", model.getIndex());
        Assertions.assertNotNull(model.getDetails());
        Assertions.assertEquals(1, model.getDetails().size());
        Assertions.assertEquals(bodyPath, model.getDetails().get("bodyPath"));
    }
}
