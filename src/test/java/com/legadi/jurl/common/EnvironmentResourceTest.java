package com.legadi.jurl.common;

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
}
