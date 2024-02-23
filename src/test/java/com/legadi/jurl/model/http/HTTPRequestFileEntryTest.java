package com.legadi.jurl.model.http;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class HTTPRequestFileEntryTest {

    @Test
    public void setterGetterValidation() {
        HTTPRequestFileEntry model = new HTTPRequestFileEntry();

        model.setName("uploaded.csv");
        model.setPath("src/test/resources/file.csv");
        model.setField("file");
        model.setMineType("text/csv");

        Map<String, String> formData = new HashMap<>();
        formData.put("identifier", UUID.randomUUID().toString());
        model.setFormData(formData);

        Assertions.assertEquals("uploaded.csv", model.getName());
        Assertions.assertEquals("src/test/resources/file.csv", model.getPath());
        Assertions.assertEquals("file", model.getField());
        Assertions.assertEquals("text/csv", model.getMineType());
        Assertions.assertEquals(1, model.getFormData().size());
        Assertions.assertDoesNotThrow(() -> UUID.fromString(model.getFormData().get("identifier")));
    }
}
