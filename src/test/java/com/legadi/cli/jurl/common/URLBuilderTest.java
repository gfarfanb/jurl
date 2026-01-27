package com.legadi.cli.jurl.common;

import java.net.MalformedURLException;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class URLBuilderTest {

    @Test
    public void buildUrl() {
        URLBuilder builder = new URLBuilder()
            .setUrl("http://localhost:1234/api/v1");

        Assertions.assertEquals("http://localhost:1234/api/v1", builder.build());
        Assertions.assertDoesNotThrow(() -> new URI(builder.build()).toURL());
    }

    @Test
    public void buildUrlIncompleteParts() {
        URLBuilder builder = new URLBuilder();

        Assertions.assertTrue(builder.build().isEmpty());
        Assertions.assertThrows(IllegalArgumentException.class,
            () -> new URI(builder.build()).toURL());
    }

    @Test
    public void buildUrlAppendQueryParams() {
        Map<String, String> queryParams = new HashMap<>();
        queryParams.put("name", "Test");
        URLBuilder builder = new URLBuilder()
            .setUrl("http://localhost:1234/api/v1?id=82hr2o3eij23r8")
            .addAllQueryParams(queryParams);

        Assertions.assertTrue(builder.build().startsWith("http://localhost:1234/api/v1?"));
        Assertions.assertTrue(builder.build().contains("id=82hr2o3eij23r8"));
        Assertions.assertTrue(builder.build().contains("name=Test"));
        Assertions.assertDoesNotThrow(() -> new URI(builder.build()).toURL());
    }

    @Test
    public void buildUrlWithQueryParams() {
        Map<String, String> queryParams = new HashMap<>();
        queryParams.put("id", "82hr2o3eij23r8");
        queryParams.put("name", "Test");
        URLBuilder builder = new URLBuilder()
            .setUrl("http://localhost:1234/api/v1")
            .addAllQueryParams(queryParams);

        Assertions.assertTrue(builder.build().startsWith("http://localhost:1234/api/v1?"));
        Assertions.assertTrue(builder.build().contains("id=82hr2o3eij23r8"));
        Assertions.assertTrue(builder.build().contains("name=Test"));
        Assertions.assertDoesNotThrow(() -> new URI(builder.build()).toURL());
    }

    @Test
    public void buildWrongUrl() {
        URLBuilder builder = new URLBuilder()
            .setUrl("http-://localhost:1234:1234/api/v1/");

        Assertions.assertEquals("http-://localhost:1234:1234/api/v1/", builder.build());
        Assertions.assertThrows(MalformedURLException.class,
            () -> new URI(builder.build()).toURL());
    }
}
