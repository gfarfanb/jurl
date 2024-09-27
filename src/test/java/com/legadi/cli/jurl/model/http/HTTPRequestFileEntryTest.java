package com.legadi.cli.jurl.model.http;

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

        Assertions.assertEquals("uploaded.csv", model.getName());
        Assertions.assertEquals("src/test/resources/file.csv", model.getPath());
        Assertions.assertEquals("file", model.getField());
        Assertions.assertEquals("text/csv", model.getMineType());
    }
}
