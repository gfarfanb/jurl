package com.legadi.cli.jurl.common;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class StringExpanderTest {

    @Test
    public void getSettingsValidation() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);

        Assertions.assertEquals(settings, expander.getSettings());
    }

    @Test
    public void replaceAllInContentValidation() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);

        settings.putOverride("name", "Test");

        Assertions.assertEquals("name: Test", expander.replaceAllInContent("name: {{name}}"));
        Assertions.assertEquals("name1: Test, name2: Test, name3: Test",
            expander.replaceAllInContent("name1: {{name}}, name2: {{name}}, name3: {{name}}"));
    }

    @Test
    public void replaceAllInContentCustomValues() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);
        Map<String, String> values = new HashMap<>();

        values.put("name", "Test");

        Assertions.assertEquals("name: Test", expander.replaceAllInContent(values, "name: {{name}}"));
        Assertions.assertEquals("name1: Test, name2: Test, name3: Test",
            expander.replaceAllInContent(values, "name1: {{name}}, name2: {{name}}, name3: {{name}}"));
    }

    @Test
    public void replaceAllInContentNull() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);
        Map<String, String> values = new HashMap<>();

        Assertions.assertNull(expander.replaceAllInContent(values, null));
    }

    @Test
    public void replaceAllInContentGenerator() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);
        Map<String, String> values = new HashMap<>();

        Assertions.assertFalse(expander.replaceAllInContent(values, "{{INTEGER:}}").isEmpty());
    }

    @Test
    public void replaceAllInContentModifier() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);
        Map<String, String> values = new HashMap<>();

        Assertions.assertEquals("0", expander.replaceAllInContent(values, "{{~default~0~number}}"));
    }

    @Test
    public void replaceAllInContentModifierNotFound() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);
        Map<String, String> values = new HashMap<>();

        Assertions.assertTrue(expander.replaceAllInContent(values, "{{~not-found~number}}").isEmpty());
    }

    @Test
    public void scanParamsInContentValidation() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);

        Set<String> params = Assertions.assertDoesNotThrow(
            () -> expander.scanParamsInContent("id: {{id}}, name: {{name}}, timestamp: {{timestamp}}"));

        Assertions.assertEquals(3, params.size());
        Assertions.assertTrue(params.contains("id"));
        Assertions.assertTrue(params.contains("name"));
        Assertions.assertTrue(params.contains("timestamp"));
    }
}
