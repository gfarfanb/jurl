package com.legadi.cli.jurl.model.http;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HTTPMockEntryTest {

    @Test
    public void setterGetterValidation() {
        HTTPMockEntry model = new HTTPMockEntry();

        Map<String, String> responseHeaders = new HashMap<>();
        responseHeaders.put("Content-Type", "application/json");
        model.setResponseHeaders(responseHeaders);

        model.setResponseContent("{\"name\": \"test\"}");
        model.setResponseFilePath("src/test/resources/mock-response.output.json");
        model.setExceptionClassOnOutputStream(IOException.class.getName());
        model.setExceptionClassOnResponseCode(IOException.class.getName());

        Assertions.assertEquals(1, model.getResponseHeaders().size());
        Assertions.assertEquals("application/json", model.getResponseHeaders().get("Content-Type"));
        Assertions.assertEquals("{\"name\": \"test\"}", model.getResponseContent());
        Assertions.assertEquals("src/test/resources/mock-response.output.json", model.getResponseFilePath());
        Assertions.assertEquals(IOException.class.getName(), model.getExceptionClassOnOutputStream());
        Assertions.assertEquals(IOException.class.getName(), model.getExceptionClassOnResponseCode());
    }
}
