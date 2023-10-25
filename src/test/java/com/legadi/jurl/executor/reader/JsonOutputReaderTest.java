package com.legadi.jurl.executor.reader;

import static com.legadi.jurl.common.JsonUtils.loadJsonFile;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.CommandException;

public class JsonOutputReaderTest {

    @Test
    public void mapOutputParams() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/id",
            "OUT/name",
            "OUT/times",
            "OUT/object.valid",
            "OUT/object.ratio",
            "OUT/object.blank",
            "OUT/elements[first].value",
            "OUT/elements[last].prefix",
            "OUT/elements[].history[any].completed",
            "OUT/elements[any].history[].completed",
            "OUT/elements[1].history[2].timestamp",
            "OUT/tags[3]",
            "OUT/tags[10]",
            "OUT/empty[3]"
        ));
        Map<String, String> output = new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/");

        Assertions.assertEquals("8ae5e442-28a6-jk3a-9412-e3kw20b3ea33", output.get("OUT/id"));
        Assertions.assertEquals("Json Output", output.get("OUT/name"));
        Assertions.assertEquals("5", output.get("OUT/times"));

        Assertions.assertEquals("true", output.get("OUT/object.valid"));
        Assertions.assertEquals("0.9", output.get("OUT/object.ratio"));
        Assertions.assertNull(output.get("OUT/object.blank"));

        Assertions.assertEquals("234.2", output.get("OUT/elements[first].value"));
        Assertions.assertEquals("Beta", output.get("OUT/elements[last].prefix"));

        Set<String> completed = new HashSet<>(Arrays.asList(
            output.get("OUT/elements[].history[any].completed"),
            output.get("OUT/elements[any].history[].completed")
        ));
        Assertions.assertEquals(1, completed.size());

        Assertions.assertEquals("2023-09-29T12:50:30.02", output.get("OUT/elements[1].history[2].timestamp"));
        Assertions.assertEquals("D", output.get("OUT/tags[3]"));
        Assertions.assertNull(output.get("OUT/tags[10]"));
        Assertions.assertNull(output.get("OUT/empty[3]"));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void mapOutputObjects() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Path outputPath = new Settings().getOutputObjectPath();
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/elements[0]",
            "OUT/elements[1].history[3]",
            "OUT/elements[2].history"
        ));
        Map<String, String> output = new JsonOutputReader().apply(sourcePath, outputPath, outputParams, "OUT/");

        Map<String, Object> element0 = loadJsonFile(output.get("OUT/elements[0]"), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("Alpha", element0.get("record"));
        Assertions.assertEquals(BigDecimal.valueOf(234.2), (BigDecimal) element0.get("value"));
        Assertions.assertEquals(2, ((List<Object>) element0.get("history")).size());

        Map<String, Object> history3 = loadJsonFile(output.get("OUT/elements[1].history[3]").toString(), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("2023-09-30T15:40:30.02", history3.get("timestamp"));
        Assertions.assertFalse((Boolean) history3.get("completed"));

        List<Object> history = loadJsonFile(output.get("OUT/elements[2].history").toString(), new TypeToken<List<Object>>() {});
        Assertions.assertEquals(3, history.size());
    }

    @Test
    public void mapOutputList() {
        Path sourcePath = Paths.get("src/test/resources/json-list.output.json");
        Path outputPath = new Settings().getOutputObjectPath();
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/[first]",
            "OUT/[0]",
            "OUT/[2]",
            "OUT/[]",
            "OUT/[any]",
            "OUT/[last]",
            "OUT/[4]"
        ));
        Map<String, String> output = new JsonOutputReader().apply(sourcePath, outputPath, outputParams, "OUT/");

        Map<String, Object> first = loadJsonFile(output.get("OUT/[first]").toString(), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("2023-09-27T07:49:30.02", first.get("timestamp"));
        Assertions.assertEquals("Begin", first.get("message"));

        Map<String, Object> element0 = loadJsonFile(output.get("OUT/[0]").toString(), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("2023-09-27T07:49:30.02", element0.get("timestamp"));
        Assertions.assertEquals("Begin", element0.get("message"));

        Map<String, Object> element2 = loadJsonFile(output.get("OUT/[2]").toString(), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("2023-09-29T07:49:30.02", element2.get("timestamp"));
        Assertions.assertEquals("Transaction B", element2.get("message"));

        Map<String, Object> any_1 = loadJsonFile(output.get("OUT/[]").toString(), new TypeToken<Map<String, Object>>() {});
        Map<String, Object> any_2 = loadJsonFile(output.get("OUT/[any]").toString(), new TypeToken<Map<String, Object>>() {});
        Set<Object> timestamps = new HashSet<>(Arrays.asList(
            any_1.get("timestamp"), any_2.get("timestamp")
        ));
        Set<Object> messages = new HashSet<>(Arrays.asList(
            any_1.get("message"), any_2.get("message")
        ));
        Assertions.assertEquals(1, timestamps.size());
        Assertions.assertEquals(1, messages.size());

        Map<String, Object> last = loadJsonFile(output.get("OUT/[last]").toString(), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("2023-10-01T15:39:30.02", last.get("timestamp"));
        Assertions.assertEquals("End", last.get("message"));

        Map<String, Object> element4 = loadJsonFile(output.get("OUT/[4]").toString(), new TypeToken<Map<String, Object>>() {});
        Assertions.assertEquals("2023-10-01T15:39:30.02", element4.get("timestamp"));
        Assertions.assertEquals("End", element4.get("message"));
    }

    @Test
    public void mapOutputArray() {
        Path sourcePath = Paths.get("src/test/resources/json-array.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/[first]",
            "OUT/[0]",
            "OUT/[2]",
            "OUT/[]",
            "OUT/[any]",
            "OUT/[last]",
            "OUT/[4]"
        ));
        Map<String, String> output = new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/");

        Assertions.assertEquals("123.23", output.get("OUT/[first]"));
        Assertions.assertEquals("123.23", output.get("OUT/[0]"));
        Assertions.assertEquals("242456.4", output.get("OUT/[2]"));

        Set<String> numbers = new HashSet<>(Arrays.asList(
            output.get("OUT/[]"), output.get("OUT/[any]")
        ));
        Assertions.assertEquals(1, numbers.size());

        Assertions.assertEquals("66757.56", output.get("OUT/[last]"));
        Assertions.assertEquals("66757.56", output.get("OUT/[4]"));
    }

    @Test
    public void paramMissing() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/missing"
        ));

        Map<String, String> output = new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/");

        Assertions.assertNull(output.get("OUT/missing"));
    }

    @Test
    public void paramDoesNotMatch() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/missing.element"
        ));

        Assertions.assertThrows(CommandException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/")
        );
    }

    @Test
    public void paramListSizeInList() {
        Path sourcePath = Paths.get("src/test/resources/json-list.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/__size__"
        ));

        Map<String, String> output = new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/");

        Assertions.assertEquals("5", output.get("OUT/__size__"));
    }

    @Test
    public void paramListSizeInObject() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/elements/__size__",
            "OUT/elements[first].history/__size__"
        ));

        Map<String, String> output = new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/");

        Assertions.assertEquals("3", output.get("OUT/elements/__size__"));
        Assertions.assertEquals("2", output.get("OUT/elements[first].history/__size__"));
    }

    @Test
    public void paramListSizeInListWrongDefinition() {
        Path sourcePath = Paths.get("src/test/resources/json-list.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT//__size__"
        ));

        Assertions.assertThrows(CommandException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/")
        );
    }

    @Test
    public void paramListSizeInObjectWrongDefinition() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/elements__size__"
        ));

        Assertions.assertThrows(CommandException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/")
        );
    }

    @Test
    public void paramListSizeInObjectNonList() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/object/__size__"
        ));

        Assertions.assertThrows(CommandException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/")
        );
    }

    @Test
    public void paramListSizeDoesNotMatch() {
        Path sourcePath = Paths.get("src/test/resources/json-object.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/missing.element/__size__"
        ));

        Assertions.assertThrows(CommandException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/")
        );
    }

    @Test
    public void invalidArrayIndex() {
        Path sourcePath = Paths.get("src/test/resources/json-array.output.json");
        Set<String> outputParams = new HashSet<>(Arrays.asList(
            "OUT/[begin]"
        ));

        Assertions.assertThrows(CommandException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, outputParams, "OUT/")
        );
    }

    @Test
    public void malformedJson() {
        Path sourcePath = Paths.get("src/test/resources/json-malformed.output.json");

        Assertions.assertThrows(IllegalStateException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, null, "OUT/")
        );
    }

    @Test
    public void notExistsJson() {
        Path sourcePath = Paths.get("src/test/resources/json-not-exists.output.json");

        Assertions.assertThrows(IllegalStateException.class, () ->
            new JsonOutputReader().apply(sourcePath, null, null, "OUT/")
        );
    }
}
