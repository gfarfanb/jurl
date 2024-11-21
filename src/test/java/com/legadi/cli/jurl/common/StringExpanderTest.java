package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.StringExpander.PropertyDefaultResolver;

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
    public void replaceAllInContentDefaults() {
        Settings settings = new Settings();

        settings.putUserInput("default.int", "5");

        Map<String, Object> defaults = new HashMap<>();
        defaults.put("name", "Test");

        StringExpander expander = new StringExpander(settings, defaults);

        Assertions.assertEquals("name: Test", expander.replaceAllInContent("name: {{name}}"));
        Assertions.assertEquals("name1: Test, name2: Test, name3: Test",
            expander.replaceAllInContent("name1: {{name}}, name2: {{name}}, name3: {{name}}"));
        Assertions.assertEquals("ID: 5", expander.replaceAllInContent("ID: {{default.int}}"));
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

        Assertions.assertEquals("0", expander.replaceAllInContent(values, "{{:default:0:number}}"));
    }

    @Test
    public void replaceAllInContentModifierNotFound() {
        Settings settings = new Settings();
        StringExpander expander = new StringExpander(settings);
        Map<String, String> values = new HashMap<>();

        Assertions.assertTrue(expander.replaceAllInContent(values, "{{:not-found:number}}").isEmpty());
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

    @Test
    public void propertyDefaultNotFound() {
        Settings settings = new Settings();
        Map<String, Object> defaults = new HashMap<>();
        PropertyDefaultResolver propertyDefaultResolver = new PropertyDefaultResolver(settings, defaults);

        String value = propertyDefaultResolver.apply("property.not.found");

        Assertions.assertEquals("", value);
    }

    @Test
    public void propertyDefaultWithDefault() {
        Settings settings = new Settings();
        Map<String, Object> defaults = new HashMap<>();

        defaults.put("default.int", "5");

        PropertyDefaultResolver propertyDefaultResolver = new PropertyDefaultResolver(settings, defaults);

        String value = propertyDefaultResolver.apply("default.int");

        Assertions.assertEquals("5", value);
    }

    @Test
    public void propertyDefaultWithListAndDefault() {
        Settings settings = new Settings();
        Map<String, Object> defaults = new HashMap<>();
        List<String> steps = new ArrayList<>();

        steps.add("1/3");
        steps.add("2/3");
        steps.add("3/3");
        defaults.put("steps", steps);
        defaults.put(getDefaultFieldIndex("steps"), "0");

        PropertyDefaultResolver propertyDefaultResolver = new PropertyDefaultResolver(settings, defaults);

        String value = propertyDefaultResolver.apply("steps");

        Assertions.assertEquals("1/3", value);
    }

    @Test
    public void propertyDefaultWithList() {
        Settings settings = new Settings();
        Map<String, Object> defaults = new HashMap<>();
        List<String> steps = new ArrayList<>();

        steps.add("1/3");
        steps.add("2/3");
        steps.add("3/3");
        defaults.put("steps", steps);

        PropertyDefaultResolver propertyDefaultResolver = new PropertyDefaultResolver(settings, defaults);

        String value = propertyDefaultResolver.apply("steps");

        Assertions.assertEquals("", value);
    }
}
