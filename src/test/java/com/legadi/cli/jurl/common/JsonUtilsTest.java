package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.JsonUtils.isArrayFile;
import static com.legadi.cli.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.cli.jurl.common.JsonUtils.loadInternalJsonProperties;
import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.JsonUtils.loadJsonProperties;
import static com.legadi.cli.jurl.common.JsonUtils.toJsonString;
import static com.legadi.cli.jurl.common.JsonUtils.writeJsonFile;
import static com.legadi.cli.jurl.common.WriterUtils.createDirectories;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.embedded.wrong.FailedFileSystemPath;

public class JsonUtilsTest {

    @Test
    public void writeJsonFileContent() throws IOException {
        Settings settings = new Settings();
        Path filePath = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/json-utils")
            )
            .resolve(UUID.randomUUID() + ".json");

        Map<String, String> content = new HashMap<>();
        content.put("name", "test");

        writeJsonFile(filePath, content);

        Assertions.assertEquals(20, filePath.toFile().length());
    }

    @Test
    public void writeJsonFileDirectoryNotFound() throws IOException {
        Settings settings = new Settings();
        Path filePath = settings.getExecutionPath()
            .resolve("directory/not/found/json-utils")
            .resolve(UUID.randomUUID() + ".json");

        Map<String, String> content = new HashMap<>();
        content.put("name", "test");

        Assertions.assertThrows(IllegalStateException.class,
            () -> writeJsonFile(filePath, content));
    }

    @Test
    public void loadJsonFileValidation() {
        Settings settings = new Settings();
        Path filePath = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/json-utils")
            )
            .resolve(UUID.randomUUID() + ".json");

        Map<String, String> content = new HashMap<>();
        content.put("name", "test");

        writeJsonFile(filePath, content);

        Map<String, String> loaded = Assertions.assertDoesNotThrow(
            () -> loadJsonFile(filePath.toString(), new TypeToken<Map<String, String>>() {}, null));

        Assertions.assertEquals("test", loaded.get("name"));
    }

    @Test
    public void loadJsonFileJsonFileNotFound() {
        Settings settings = new Settings();
        Path filePath = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/json-utils")
            )
            .resolve("json-file-not-found.json");

        Map<String, String> input = Assertions.assertDoesNotThrow(
            () -> loadJsonFile(filePath.toString(), new TypeToken<Map<String, String>>() {}, null));

        Assertions.assertNull(input);
    }

    @Test
    public void loadJsonFileInvalidParseType() {
        Settings settings = new Settings();
        Path filePath = createDirectories(
                settings.getExecutionPath().resolve("src/test/resources/json-utils")
            )
            .resolve(UUID.randomUUID() + ".json");

        Map<String, String> content = new HashMap<>();
        content.put("name", "test");

        writeJsonFile(filePath, content);

        Assertions.assertThrows(IllegalStateException.class,
            () -> loadJsonFile(filePath.toString(), null, null));
    }

    @Test
    public void loadInternalJsonPropertiesValidation() {
        Map<String, String> properties = Assertions.assertDoesNotThrow(
            () -> loadInternalJsonProperties("json-properties.json"));

        Assertions.assertEquals("json/utils/test", properties.get("path"));
    }

    @Test
    public void loadInternalJsonPropertiesFileNotFound() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> loadInternalJsonProperties("json-file-not-found.json"));
    }

    @Test
    public void loadJsonPropertiesValidation() {
        Path filePath = Paths.get("src/test/resources/json-properties.json");

        Map<String, String> properties = Assertions.assertDoesNotThrow(
            () -> loadJsonProperties(filePath));

        Assertions.assertEquals("json/utils/test", properties.get("path"));
    }

    @Test
    public void loadJsonPropertiesFileNotFound() {
        Path filePath = Paths.get("json-file-not-found.json");

        Map<String, String> properties = Assertions.assertDoesNotThrow(
            () -> loadJsonProperties(filePath));

        Assertions.assertTrue(properties.isEmpty());
    }

    @Test
    public void jsonToObjectValidation() {
        Map<String, Object> object = Assertions.assertDoesNotThrow(
            () -> jsonToObject("{\"name\": \"test\"}", new TypeToken<Map<String, Object>>() {}));

        Assertions.assertEquals("test", object.get("name"));
    }

    @Test
    public void jsonToObjectInvalid() {
        Assertions.assertThrows(IllegalStateException.class,
            () -> jsonToObject("name: test", new TypeToken<Map<String, Object>>() {}));
    }

    @Test
    public void toJsonStringValidation() {
        Map<String, String> content = new HashMap<>();
        content.put("name", "test");

        String json = Assertions.assertDoesNotThrow(
            () -> toJsonString(content));

        Assertions.assertEquals("{\n  \"name\": \"test\"\n}", json);

        String jsonString = Assertions.assertDoesNotThrow(
            () -> toJsonString("{\"name\": \"test\"}"));

        Assertions.assertEquals("\"{\\\"name\\\": \\\"test\\\"}\"", jsonString);

        String nullString = Assertions.assertDoesNotThrow(
            () -> toJsonString(null));

        Assertions.assertEquals("null", nullString);
    }

    @Test
    public void isArrayFileForObject() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");

        Assertions.assertFalse(isArrayFile(sourcePath));
    }

    @Test
    public void isArrayFileForArray() {
        Path sourcePath = Paths.get("src/test/resources/json-array.output.json");

        Assertions.assertTrue(isArrayFile(sourcePath));
    }

    @Test
    public void isArrayFileMalformed() {
        Path sourcePath = Paths.get("src/test/resources/json-malformed.output.json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> isArrayFile(sourcePath));
    }

    @Test
    public void isArrayFileFailed() {
        Path sourcePath = Paths.get("src/test/resources/json-malformed.output.json");

        Assertions.assertThrows(IllegalStateException.class,
            () -> isArrayFile(new FailedFileSystemPath(sourcePath)));
    }
}
