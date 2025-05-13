package com.legadi.cli.jurl.executor.decoder;

import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;

public class GzipOutputDecoderTest {

    @Test
    public void decodeValidation() {
        OutputDecoder decoder = findOrFail(OutputDecoder.class, "gzip");

        Path outputPath = Assertions.assertDoesNotThrow(
            () -> decoder.apply(Paths.get("src/test/resources/json-decoder.output.gz")));

        Map<String, String> output = loadJsonFile(outputPath.toString() , new TypeToken<Map<String, String>>() {}, null);

        Assertions.assertEquals("Successful", output.get("status"));
    }

    @Test
    public void decodeNotFound() {
        OutputDecoder decoder = findOrFail(OutputDecoder.class, "gzip");

        Path outputPath = Assertions.assertDoesNotThrow(
            () -> decoder.apply(Paths.get("src/test/resources/json-decoder.not-found.gz")));

        Assertions.assertEquals(Paths.get("src/test/resources/json-decoder.not-found.gz"), outputPath);
    }

    @Test
    public void decodeNullPath() {
        OutputDecoder decoder = findOrFail(OutputDecoder.class, "gzip");

        Path outputPath = Assertions.assertDoesNotThrow(
            () -> decoder.apply(null));

        Assertions.assertNull(outputPath);
    }
}
