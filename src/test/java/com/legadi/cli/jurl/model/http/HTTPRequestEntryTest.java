package com.legadi.cli.jurl.model.http;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HTTPRequestEntryTest {

    @Test
    public void setterGetterValidation() {
        HTTPRequestEntry model = new HTTPRequestEntry();

        model.setMethod("POST");

        Map<String, String> queryParams = new HashMap<>();
        queryParams.put("search", "value");
        model.setQueryParams(queryParams);

        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");
        model.setHeaders(headers);

        model.setBodyCharset(StandardCharsets.UTF_8.name());
        model.setBodyContent("{\"name\": \"test\"}");
        model.setBodyFilePath("src/test/resources/basic-functions.body.json");

        List<HTTPRequestFileEntry> requestFiles = new ArrayList<>();
        requestFiles.add(new HTTPRequestFileEntry());
        model.setRequestFiles(requestFiles);

        model.setRequestAuth(new HTTPRequestAuthEntry());

        Map<String, String> formData = new HashMap<>();
        formData.put("identifier", UUID.randomUUID().toString());
        model.setFormData(formData);

        Map<String, String> defaults = new HashMap<>();
        defaults.put("local.port", "2342");
        model.setDefaults(defaults);

        Assertions.assertEquals("POST", model.getMethod());
        Assertions.assertEquals(1, model.getQueryParams().size());
        Assertions.assertEquals("value", model.getQueryParams().get("search"));
        Assertions.assertEquals(1, model.getHeaders().size());
        Assertions.assertEquals("application/json", model.getHeaders().get("Content-Type"));
        Assertions.assertEquals(StandardCharsets.UTF_8.name(), model.getBodyCharset());
        Assertions.assertEquals("{\"name\": \"test\"}", model.getBodyContent());
        Assertions.assertEquals("src/test/resources/basic-functions.body.json", model.getBodyFilePath());
        Assertions.assertFalse(model.getRequestFiles().isEmpty());
        Assertions.assertNotNull(model.getRequestAuth());
        Assertions.assertEquals(1, model.getFormData().size());
        Assertions.assertEquals(1, model.getDefaults().size());
        Assertions.assertEquals("2342", model.getDefaults().get("local.port"));
        Assertions.assertDoesNotThrow(() -> UUID.fromString(model.getFormData().get("identifier")));
    }
}
