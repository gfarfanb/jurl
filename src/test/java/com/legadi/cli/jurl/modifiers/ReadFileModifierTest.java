package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.JsonUtils.jsonToObject;

import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;

public class ReadFileModifierTest extends ValueModifierAbstractTest<ReadFileModifier> {

    public ReadFileModifierTest() {
        super("read_file");
    }

    @Override
    public String[] sampleValidArgs() {
        return new String[0];
    }

    @Test
    public void readFile() {
        String[] args = {};
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "src/test/resources/json-body.json"));
        Map<String, String> body = jsonToObject(result, new TypeToken<Map<String, String>>() {});

        Assertions.assertEquals("json", body.get("type"));
    }

    @Test
    public void readFileNotFound() {
        String[] args = {};
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, "src/test/resources/not-found-body.json"));

        Assertions.assertEquals("src/test/resources/not-found-body.json", result);
    }

    @Test
    public void readFileException() {
        String[] args = {};
        String result = Assertions.assertDoesNotThrow(
            () -> apply(args, null));

        Assertions.assertNull(result);
    }
}
