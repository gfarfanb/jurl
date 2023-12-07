package com.legadi.jurl.common;

import static com.legadi.jurl.common.Settings.TAG_FORMATTER;
import static com.legadi.jurl.common.Settings.mergeProperties;

import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.model.RequestBehaviour;

public class SettingsTest {

    @Test
    public void getCommandProperties() {
        Settings settings = new Settings();

        Assertions.assertEquals(System.getProperty("user.dir"), settings.get("workspacePath"));
        Assertions.assertDoesNotThrow(() -> TAG_FORMATTER.parse(settings.get("executionTag")));
    }

    @Test
    public void getDefaults() {
        Settings settings = new Settings();

        Assertions.assertEquals(Paths.get("./config/"), settings.getConfigPath());
        Assertions.assertEquals(Paths.get("./config/.output/"), settings.getConfigOutputPath());
        Assertions.assertEquals(Paths.get("./executions/"), settings.getExecutionPath());
        Assertions.assertEquals(Paths.get("./history/"), settings.getHistoryPath());
        Assertions.assertEquals(1, settings.getAddOnOptionClasses().length);
        Assertions.assertTrue(settings.getOpenEditorCommand().isEmpty());
        Assertions.assertEquals(RequestBehaviour.REQUEST, settings.getRequestBehaviour());
        Assertions.assertFalse(settings.isMockRequest());
        Assertions.assertFalse(settings.isOpenOutputInEditor());
        Assertions.assertEquals(1, settings.getTimes());
        Assertions.assertTrue(settings.getInputName().isEmpty());
        Assertions.assertEquals("(?:\\{\\{([\\w.,\\-:\\'@~|\\[\\]/\\\\]+)\\}\\})", settings.getSettingsParamRegex());
        Assertions.assertEquals("\\{\\{", settings.getSettingsParamRegexBegin());
        Assertions.assertEquals("\\}\\}", settings.getSettingsParamRegexEnd());
        Assertions.assertFalse(settings.isSkipAssertions());
        Assertions.assertTrue(settings.getMergeBodyUsingType().isEmpty());
        Assertions.assertTrue(settings.getOverrideRequestFilePath().isEmpty());
        Assertions.assertArrayEquals(new String[] {
                "application/graphql+json",
                "application/javascript",
                "application/vnd.mozilla.xul+xml",
                "application/x-www-form-urlencoded",
                "application/xhtml+xml",
                "application/xml",
                "text/css",
                "text/csv",
                "text/html",
                "text/javascript",
                "text/plain",
                "text/xml"
            },
            settings.getPrintableMimeTypes());
    }

    @Test
    public void getTimestampValidation() {
        LocalDateTime now = LocalDateTime.now();
        Settings settings = new Settings();

        Assertions.assertTrue(settings.getTimestamp().compareTo(now) >= 0);
    }

    @Test
    public void getExecutionTagValidation() {
        LocalDateTime now = LocalDateTime.now();
        Settings settings = new Settings();

        Assertions.assertTrue(settings.getExecutionTag().startsWith(now.toLocalDate() + "."));
    }

    @Test
    public void getEnvironmentValidation() {
        Settings settings = new Settings();

        Assertions.assertEquals("default", settings.getEnvironment());

        settings.setEnvironment("test");

        Assertions.assertEquals("test", settings.getEnvironment());
    }

    @Test
    public void getValidation() {
        Settings settings = new Settings();

        Assertions.assertEquals("REQUEST", settings.get("requestBehaviour"));
        Assertions.assertEquals(Integer.valueOf(1), settings.get("executionTimes", Integer::parseInt));
        Assertions.assertThrows(CommandException.class, () -> settings.get("property.not.found"));
    }

    @Test
    public void getOrDefaultValidation() {
        Settings settings = new Settings();

        Assertions.assertEquals("REQUEST", settings.getOrDefault("request.behaviour", "REQUEST"));
        Assertions.assertEquals(1, settings.getOrDefault("execution.times", 1, Integer::parseInt));
        Assertions.assertEquals(1, settings.getOrDefault("executionTimes", 5, Integer::parseInt));
    }

    @Test
    public void getConfigFilePathValidation() {
        Settings settings = new Settings();
        String configPath = settings.get("configPath");

        Assertions.assertEquals(Paths.get("./config/config.json"), settings.getConfigFilePath());

        settings.setEnvironment("test");
        settings.putOverride("configPath", configPath);
        
        Assertions.assertEquals(Paths.get("./config/config.test.json"), settings.getConfigFilePath());
    }

    @Test
    public void getOverrideFilePathValidation() {
        Settings settings = new Settings();
        String configOutputPath = settings.get("configOutputPath");

        Assertions.assertEquals(Paths.get("./config/.output/override.json"), settings.getOverrideFilePath());

        settings.setEnvironment("test");
        settings.putOverride("configOutputPath", configOutputPath);
        
        Assertions.assertEquals(Paths.get("./config/.output/override.test.json"), settings.getOverrideFilePath());
    }

    @Test
    public void getOutputObjectPathValidation() {
        Settings settings = new Settings();
        String configOutputPath = settings.get("configOutputPath");

        Assertions.assertEquals(Paths.get("./config/.output/default"), settings.getOutputObjectPath());

        settings.setEnvironment("test");
        settings.putOverride("configOutputPath", configOutputPath);
        
        Assertions.assertEquals(Paths.get("./config/.output/test"), settings.getOutputObjectPath());
    }

    @Test
    public void containsAndPutOverrideValidation() {
        Settings settings = new Settings();

        Assertions.assertFalse(settings.containsOverride("property.not.found"));

        settings.putOverride("property.not.found", "test");

        Assertions.assertTrue(settings.containsOverride("property.not.found"));
    }

    @Test
    public void createForNextExecutionValidation() {
        Settings settings = new Settings();

        settings.putOverride("override.property", "test");

        Settings nextSettings = settings.createForNextExecution();

        settings.putOverride("override.property", "overrided");

        Assertions.assertFalse(settings.equals(nextSettings));
        Assertions.assertEquals("overrided", settings.get("override.property"));
        Assertions.assertEquals("test", nextSettings.get("override.property"));
    }

    @Test
    public void mergePropertiesValidation() {
        Map<String, String> properties = new HashMap<>();

        properties.put("property.1", "1");
        properties.put("property.2", "2");
        properties.put("property.3", "3");

        Assertions.assertDoesNotThrow(() -> mergeProperties("default", properties));

        Settings settings = new Settings();

        Assertions.assertEquals("1", settings.get("property.1"));
        Assertions.assertEquals("2", settings.get("property.2"));
        Assertions.assertEquals("3", settings.get("property.3"));
    }
}
