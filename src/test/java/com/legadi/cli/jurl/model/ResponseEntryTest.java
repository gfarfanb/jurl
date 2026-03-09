package com.legadi.cli.jurl.model;

import static com.legadi.cli.jurl.common.SettingsConstants.TAG_FORMATTER;
import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;

import java.time.LocalDateTime;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ResponseEntryTest {

    @Test
    public void setterGetterValidation() {
        ResponseEntry model = new ResponseEntry();
        UUID uuid = UUID.randomUUID();
        LocalDateTime now = LocalDateTime.now();
        String curl = "curl -X POST"
            + " -H \"Request-Catcher: " + uuid + "\""
            + " -H \"Content-Type: application/json\""
            + " --data-binary \"@./executions/src/test/resources/api-definition.http/requests-name/"
            + ISO_LOCAL_DATE.format(now) + "/" + TAG_FORMATTER.format(now) + ".body\""
            + " \"http://localhost:42121/basic/body\"";

        model.setRequestUrl("http://localhost:42121/basic/body");
        model.setCurlCommand(curl);
        model.setResult("HTTP/1.1 201 OK");

        Assertions.assertEquals("http://localhost:42121/basic/body", model.getRequestUrl());
        Assertions.assertEquals(curl, model.getCurlCommand());
        Assertions.assertEquals("HTTP/1.1 201 OK", model.getResult());
    }
}
