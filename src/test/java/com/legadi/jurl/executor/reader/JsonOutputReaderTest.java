package com.legadi.jurl.executor.reader;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.common.Settings;

public class JsonOutputReaderTest {

    @Test
    public void mapOutputParams() {
        Path sourcePath = Paths.get("src/test/resources/json-object-output.json");
        Path outputPath = new Settings().getExecutionOutputPath();
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT:name",
            "OUT:elements[first].value",
            "OUT:elements.history[first].completed",
            "OUT:elements[].history[last].completed",
            "OUT:tags[3]",
            "OUT:empty"
        ));
        Map<String, String> output = new JsonOutputReader().apply(sourcePath, outputPath, outputParams, "OUT:");

        Assertions.assertEquals("Json Output Input", output.get("OUT:name"));
        Assertions.assertEquals("234.2", output.get("OUT:elements[first].value"));
        Assertions.assertNotNull(output.get("OUT:elements.history[first].completed"));
        Assertions.assertNotNull(output.get("OUT:elements[].history[last].completed"));
        Assertions.assertEquals("D", output.get("OUT:tags[3]"));
        Assertions.assertNull(output.get("OUT:empty"));
    }
}
