package com.legadi.cli.jurl.common;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class EnvironmentResourceTest {

    @Test
    public void loadAndGet() {
        EnvironmentResource<String> environmentResource = new EnvironmentResource<>();
        Map<String, String> resources = new HashMap<>();
        resources.put("a", "A");
        resources.put("b", "B");
        resources.put("c", "C");

        environmentResource.putAll("test", resources);

        Assertions.assertEquals("A", environmentResource.get("test", "a"));
        Assertions.assertEquals("D", environmentResource.getOrDefault("test", "d", "D"));
    }

    @Test
    public void loadAndGetInCommon() {
        EnvironmentResource<String> environmentResource = new EnvironmentResource<>();
        Map<String, String> resources = new HashMap<>();
        resources.put("1", "1");
        resources.put("2", "2");
        resources.put("3", "3");

        environmentResource.putAllInCommon(resources);

        Assertions.assertEquals("1", environmentResource.get("non-existent", "1"));
    }

    @Test
    public void loadAndGetNull() {
        EnvironmentResource<String> environmentResource = new EnvironmentResource<>();
        Map<String, String> resources = new HashMap<>();
        resources.put("a", "A");
        resources.put("b", "B");
        resources.put("c", "C");

        environmentResource.putAll(null, resources);

        Assertions.assertEquals("A", environmentResource.get(null, "a"));
        Assertions.assertEquals("D", environmentResource.getOrDefault(null, "d", "D"));
    }

    @Test
    public void putAllNull() {
        EnvironmentResource<String> environmentResource = new EnvironmentResource<>();

        Assertions.assertDoesNotThrow(() -> environmentResource.putAll("test", null));
    }

    @Test
    public void putAllInCommonNull() {
        EnvironmentResource<String> environmentResource = new EnvironmentResource<>();

        Assertions.assertDoesNotThrow(() -> environmentResource.putAllInCommon(null));
    }
}
