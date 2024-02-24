package com.legadi.jurl.executor.mixer;

import static com.legadi.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.jurl.common.ObjectsRegistry.findOrFail;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.embedded.model.ListInputEntry;
import com.legadi.jurl.embedded.model.ObjectInput;
import com.legadi.jurl.exception.InvalidInputEntryException;
import com.legadi.jurl.executor.mixer.BodyMixer.MixerEntry;

public class JsonBodyMixerTest {

    @Test
    public void mergeObjectListReplace() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-object.input.json")
            .setBodyContent("{"
                + "log: ["
                    + "{"
                        + "\"timestamp\": \"2023-10-09T18:13:30.02\","
                        + "\"completed\": true"
                    + "}"
                + "]"
            +"}")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-object"));

        ObjectInput input = loadJsonFile(mergedPath.toString(), new TypeToken<ObjectInput>() {});

        Assertions.assertEquals("Json Input", input.getName());
        Assertions.assertEquals("NEW", input.getType());
        Assertions.assertEquals(BigDecimal.valueOf(557457.23), input.getValue());

        Assertions.assertEquals(1, input.getLog().size());

        Assertions.assertEquals(LocalDateTime.from(DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse("2023-10-09T18:13:30.020")),
            input.getLog().get(0).getTimestamp());
        Assertions.assertEquals(true, input.getLog().get(0).isCompleted());
    }

    @Test
    public void mergeObjectListAddEnd() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-object.input.json")
            .setBodyContent("{"
                + "log: ["
                    + "{"
                        + "\"listMergeRule\": \"ADD_END\""
                    + "},"
                    + "{"
                        + "\"timestamp\": \"2023-10-09T18:13:30.02\","
                        + "\"completed\": true"
                    + "}"
                + "]"
            +"}")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-object"));

        ObjectInput input = loadJsonFile(mergedPath.toString(), new TypeToken<ObjectInput>() {});

        Assertions.assertEquals("Json Input", input.getName());
        Assertions.assertEquals("NEW", input.getType());
        Assertions.assertEquals(BigDecimal.valueOf(557457.23), input.getValue());

        Assertions.assertEquals(4, input.getLog().size());

        Assertions.assertEquals(LocalDateTime.from(DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse("2023-09-27T07:49:30.02")),
            input.getLog().get(0).getTimestamp());
        Assertions.assertEquals(false, input.getLog().get(0).isCompleted());

        Assertions.assertEquals(LocalDateTime.from(DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse("2023-10-09T18:13:30.02")),
            input.getLog().get(3).getTimestamp());
        Assertions.assertEquals(true, input.getLog().get(3).isCompleted());
    }

    @Test
    public void mergeListReplace() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"name\": \"Json Input Test\","
                    + "\"type\": \"TEST\","
                    + "\"value\": 0.0,"
                    + "\"properties\": {"
                        + "\"enable\": false,"
                        + "\"delay\": -5"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(1, entries.size());

        Assertions.assertEquals("Json Input Test", entries.get(0).getName());
        Assertions.assertEquals("TEST", entries.get(0).getType());
        Assertions.assertEquals(BigDecimal.valueOf(0.0), entries.get(0).getValue());
        Assertions.assertEquals(false, entries.get(0).getProperties().isEnable());
        Assertions.assertEquals(-5, entries.get(0).getProperties().getDelay());
    }

    @Test
    public void mergeListReplaceExplicit() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"REPLACE\""
                + "},"
                + "{"
                    + "\"name\": \"Json Input Test Explicit\","
                    + "\"type\": \"EXPLICIT\","
                    + "\"value\": 1.5,"
                    + "\"properties\": {"
                        + "\"enable\": true,"
                        + "\"delay\": -1"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(1, entries.size());

        Assertions.assertEquals("Json Input Test Explicit", entries.get(0).getName());
        Assertions.assertEquals("EXPLICIT", entries.get(0).getType());
        Assertions.assertEquals(BigDecimal.valueOf(1.5), entries.get(0).getValue());
        Assertions.assertEquals(true, entries.get(0).getProperties().isEnable());
        Assertions.assertEquals(-1, entries.get(0).getProperties().getDelay());
    }

    @Test
    public void mergeListReplaceEmpty() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("[]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(0, entries.size());
    }

    @Test
    public void mergeListAddBeginExplicit() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"ADD_BEGIN\""
                + "},"
                + "{"
                    + "\"name\": \"Json Input Test Explicit\","
                    + "\"type\": \"EXPLICIT\","
                    + "\"value\": 1.5,"
                    + "\"properties\": {"
                        + "\"enable\": true,"
                        + "\"delay\": -1"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(4, entries.size());

        Assertions.assertEquals("Json Input Test Explicit", entries.get(0).getName());
        Assertions.assertEquals("EXPLICIT", entries.get(0).getType());
        Assertions.assertEquals(BigDecimal.valueOf(1.5), entries.get(0).getValue());
        Assertions.assertEquals(true, entries.get(0).getProperties().isEnable());
        Assertions.assertEquals(-1, entries.get(0).getProperties().getDelay());

        Assertions.assertEquals("Json Input [2]", entries.get(3).getName());
        Assertions.assertEquals("REMOVED", entries.get(3).getType());
        Assertions.assertEquals(BigDecimal.valueOf(2456.36), entries.get(3).getValue());
        Assertions.assertEquals(false, entries.get(3).getProperties().isEnable());
        Assertions.assertEquals(0, entries.get(3).getProperties().getDelay());
    }

    @Test
    public void mergeListAddEndExplicit() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"ADD_END\""
                + "},"
                + "{"
                    + "\"name\": \"Json Input Test Explicit\","
                    + "\"type\": \"EXPLICIT\","
                    + "\"value\": 1.5,"
                    + "\"properties\": {"
                        + "\"enable\": true,"
                        + "\"delay\": -1"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(4, entries.size());

        Assertions.assertEquals("Json Input [0]", entries.get(0).getName());
        Assertions.assertEquals("NEW", entries.get(0).getType());
        Assertions.assertEquals(BigDecimal.valueOf(32432.34), entries.get(0).getValue());
        Assertions.assertEquals(true, entries.get(0).getProperties().isEnable());
        Assertions.assertEquals(5, entries.get(0).getProperties().getDelay());

        Assertions.assertEquals("Json Input Test Explicit", entries.get(3).getName());
        Assertions.assertEquals("EXPLICIT", entries.get(3).getType());
        Assertions.assertEquals(BigDecimal.valueOf(1.5), entries.get(3).getValue());
        Assertions.assertEquals(true, entries.get(3).getProperties().isEnable());
        Assertions.assertEquals(-1, entries.get(3).getProperties().getDelay());
    }

    @Test
    public void mergeListMergeExplicit() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"MERGE\","
                    + "\"keyFields\": [\"name\"]"
                + "},"
                + "{"
                    + "\"name\": \"Json Input [2]\","
                    + "\"type\": \"RETURNED\","
                    + "\"properties\": {"
                        + "\"delay\": 5"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(3, entries.size());

        Assertions.assertEquals("Json Input [2]", entries.get(2).getName());
        Assertions.assertEquals("RETURNED", entries.get(2).getType());
        Assertions.assertEquals(BigDecimal.valueOf(2456.36), entries.get(2).getValue());
        Assertions.assertEquals(false, entries.get(2).getProperties().isEnable());
        Assertions.assertEquals(5, entries.get(2).getProperties().getDelay());
    }

    @Test
    public void mergeListMergeKeyNotFound() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"MERGE\","
                    + "\"keyFields\": [\"name\"]"
                + "},"
                + "{"
                    + "\"name\": \"Json Input New\","
                    + "\"type\": \"ANOTHER\","
                    + "\"properties\": {"
                        + "\"delay\": 10"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(3, entries.size());

        Assertions.assertEquals("Json Input [0]", entries.get(0).getName());
        Assertions.assertEquals("NEW", entries.get(0).getType());
        Assertions.assertEquals(BigDecimal.valueOf(32432.34), entries.get(0).getValue());
        Assertions.assertEquals(true, entries.get(0).getProperties().isEnable());
        Assertions.assertEquals(5, entries.get(0).getProperties().getDelay());

        Assertions.assertEquals("Json Input [1]", entries.get(1).getName());
        Assertions.assertEquals("UPDATED", entries.get(1).getType());
        Assertions.assertEquals(BigDecimal.valueOf(323.35), entries.get(1).getValue());
        Assertions.assertEquals(true, entries.get(1).getProperties().isEnable());
        Assertions.assertEquals(1, entries.get(1).getProperties().getDelay());

        Assertions.assertEquals("Json Input [2]", entries.get(2).getName());
        Assertions.assertEquals("REMOVED", entries.get(2).getType());
        Assertions.assertEquals(BigDecimal.valueOf(2456.36), entries.get(2).getValue());
        Assertions.assertEquals(false, entries.get(2).getProperties().isEnable());
        Assertions.assertEquals(0, entries.get(2).getProperties().getDelay());
    }

    @Test
    public void mergeListUnknownRule() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();

        Assertions.assertThrows(InvalidInputEntryException.class,
            () -> mixer.apply(settings, new MixerEntry()
                .setBodyFilePath("src/test/resources/json-list.input.json")
                .setBodyContent("["
                    + "{"
                        + "\"listMergeRule\": \"UNKNOWN\""
                    + "},"
                    + "{"
                        + "\"name\": \"Json Input [2]\","
                        + "\"type\": \"RETURNED\","
                        + "\"properties\": {"
                            + "\"delay\": 5"
                        + "}"
                    + "}"
                +"]")
                .setRequestPath("src/test/resources/json-body-mixer.json")
                .setRequestName("mixer-list"))
        );
    }

    @Test
    public void mergeListEmptyFirstElement() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                + "},"
                + "{"
                    + "\"name\": \"Json Input New\","
                    + "\"type\": \"ANOTHER\","
                    + "\"properties\": {"
                        + "\"delay\": 10"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(2, entries.size());

        Assertions.assertNull(entries.get(0).getName());
        Assertions.assertNull(entries.get(0).getType());
        Assertions.assertNull(entries.get(0).getValue());
        Assertions.assertNull(entries.get(0).getProperties());

        Assertions.assertEquals("Json Input New", entries.get(1).getName());
        Assertions.assertEquals("ANOTHER", entries.get(1).getType());
        Assertions.assertNull(entries.get(1).getValue());
        Assertions.assertEquals(false, entries.get(1).getProperties().isEnable());
        Assertions.assertEquals(10, entries.get(1).getProperties().getDelay());
    }

    @Test
    public void mergeListMissingListMergeRule() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();

        Assertions.assertThrows(InvalidInputEntryException.class,
            () -> mixer.apply(settings, new MixerEntry()
                .setBodyFilePath("src/test/resources/json-list.input.json")
                .setBodyContent("["
                    + "{"
                        + "\"keyFields\": [\"name\"]"
                    + "},"
                    + "{"
                        + "\"name\": \"Json Input [2]\","
                        + "\"type\": \"RETURNED\","
                        + "\"properties\": {"
                            + "\"delay\": 5"
                        + "}"
                    + "}"
                +"]")
                .setRequestPath("src/test/resources/json-body-mixer.json")
                .setRequestName("mixer-list"))
        );
    }

    @Test
    public void mergeListKeyFieldsNull() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-list.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"MERGE\","
                    + "\"keyFields\": null"
                + "},"
                + "{"
                    + "\"name\": \"Json Input New\","
                    + "\"type\": \"ANOTHER\","
                    + "\"properties\": {"
                        + "\"delay\": 10"
                    + "}"
                + "}"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<ListInputEntry> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<ListInputEntry>>() {});

        Assertions.assertEquals(4, entries.size());

        Assertions.assertEquals("Json Input New", entries.get(3).getName());
        Assertions.assertEquals("ANOTHER", entries.get(3).getType());
        Assertions.assertNull(entries.get(3).getValue());
        Assertions.assertEquals(false, entries.get(3).getProperties().isEnable());
        Assertions.assertEquals(10, entries.get(3).getProperties().getDelay());
    }

    @Test
    public void mergeListArrayValues() {
        BodyMixer mixer = findOrFail(BodyMixer.class, "json");
        Settings settings = new Settings();
        Path mergedPath = mixer.apply(settings, new MixerEntry()
            .setBodyFilePath("src/test/resources/json-array.input.json")
            .setBodyContent("["
                + "{"
                    + "\"listMergeRule\": \"MERGE\","
                    + "\"keyFields\": [\"name\"]"
                + "},"
                + "ANOTHER"
            +"]")
            .setRequestPath("src/test/resources/json-body-mixer.json")
            .setRequestName("mixer-list"));

        List<String> entries = loadJsonFile(mergedPath.toString(), new TypeToken<List<String>>() {});

        Assertions.assertEquals(4, entries.size());
        Assertions.assertTrue(entries.contains("ANOTHER"));
    }
}
