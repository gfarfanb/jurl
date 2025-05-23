package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.ALPHA_NUMERIC_STRING;
import static com.legadi.cli.jurl.common.CommonUtils.NUMERIC_STRING;
import static com.legadi.cli.jurl.common.CommonUtils.avoidFirstZero;
import static com.legadi.cli.jurl.common.CommonUtils.fileSeparatorAsDelimiter;
import static com.legadi.cli.jurl.common.CommonUtils.formatAsOrderedList;
import static com.legadi.cli.jurl.common.CommonUtils.formatInColumns;
import static com.legadi.cli.jurl.common.CommonUtils.getAllFields;
import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;
import static com.legadi.cli.jurl.common.CommonUtils.getOrDefault;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.cli.jurl.common.CommonUtils.toGroupParam;
import static com.legadi.cli.jurl.common.CommonUtils.isNotNumeric;
import static com.legadi.cli.jurl.common.CommonUtils.isNumeric;
import static com.legadi.cli.jurl.common.CommonUtils.nextIndex;
import static com.legadi.cli.jurl.common.CommonUtils.nextNumber;
import static com.legadi.cli.jurl.common.CommonUtils.nextString;
import static com.legadi.cli.jurl.common.CommonUtils.selectActive;
import static com.legadi.cli.jurl.common.CommonUtils.strip;
import static com.legadi.cli.jurl.common.CommonUtils.stripEnd;
import static com.legadi.cli.jurl.common.CommonUtils.stripStart;
import static com.legadi.cli.jurl.common.CommonUtils.toIndex;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
import static com.legadi.cli.jurl.common.SettingsConstants.JURL_FILE_SEPARATOR;

import java.io.File;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import com.legadi.cli.jurl.model.GroupConfig;

public class CommonUtilsTest {

    private static final String SYMBOLS = "|!\"#$%&/()=?¡'¿°¬´+¨*~{}[]^`,.-;:_";

    @Test
    public void isNotBlankValidation() {
        Assertions.assertTrue(isNotBlank("a"));
        Assertions.assertTrue(isNotBlank(" a"));
        Assertions.assertTrue(isNotBlank(" a "));
        Assertions.assertFalse(isNotBlank(null));
        Assertions.assertFalse(isNotBlank(""));
        Assertions.assertFalse(isNotBlank(" "));
    }

    @Test
    public void isBlankValidation() {
        Assertions.assertTrue(isBlank(null));
        Assertions.assertTrue(isBlank(""));
        Assertions.assertTrue(isBlank(" "));
        Assertions.assertFalse(isBlank("a"));
        Assertions.assertFalse(isBlank(" a"));
        Assertions.assertFalse(isBlank(" a "));
    }

    @Test
    public void isNotEmptyCollectionValidation() {
        Collection<String> collection = null;

        Assertions.assertTrue(isNotEmpty(Arrays.asList("a")));
        Assertions.assertFalse(isNotEmpty(collection));
        Assertions.assertFalse(isNotEmpty(Collections.emptyList()));
    }

    @Test
    public void isEmptyCollectionValidation() {
        Collection<String> collection = null;

        Assertions.assertTrue(isEmpty(collection));
        Assertions.assertTrue(isEmpty(Collections.emptyList()));
        Assertions.assertFalse(isEmpty(Arrays.asList("a")));
    }

    @Test
    public void isNotEmptyMapValidation() {
        Map<String, String> map = new HashMap<>();
        Map<String, String> mapNull = null;
        map.put("a", "A");

        Assertions.assertTrue(isNotEmpty(map));
        Assertions.assertFalse(isNotEmpty(mapNull));
        Assertions.assertFalse(isNotEmpty(Collections.emptyMap()));
    }

    @Test
    public void isEmptyMapValidation() {
        Map<String, String> map = new HashMap<>();
        Map<String, String> mapNull = null;
        map.put("a", "A");

        Assertions.assertTrue(isEmpty(mapNull));
        Assertions.assertTrue(isEmpty(Collections.emptyMap()));
        Assertions.assertFalse(isEmpty(map));
    }

    @Test
    public void isNotEmptyArrayValidation() {
        String[] array = null;

        Assertions.assertTrue(isNotEmpty(new String[] { "a" }));
        Assertions.assertFalse(isNotEmpty(array));
        Assertions.assertFalse(isNotEmpty(new String[0]));
    }

    @Test
    public void isEmptyArrayValidation() {
        String[] array = null;

        Assertions.assertTrue(isEmpty(array));
        Assertions.assertTrue(isEmpty(new String[0]));
        Assertions.assertFalse(isEmpty(new String[] { "a" }));
    }

    @Test
    public void trimValidation() {
        Assertions.assertNull(trim(null));
        Assertions.assertEquals("", trim(""));
        Assertions.assertEquals("a", trim(" a "));
        Assertions.assertEquals(". a .", trim(". a ."));
    }

    @Test
    public void fileSeparatorAsDelimiterValidation() {
        Settings settings = new Settings();
        String delimiter = fileSeparatorAsDelimiter(settings);

        Assertions.assertNotNull(delimiter);

        System.setProperty(JURL_FILE_SEPARATOR, "\\");

        delimiter = fileSeparatorAsDelimiter(settings);
        Assertions.assertEquals("\\\\", delimiter);

        System.setProperty(JURL_FILE_SEPARATOR, "/");

        delimiter = fileSeparatorAsDelimiter(settings);
        Assertions.assertEquals("/", delimiter);

        System.setProperty(JURL_FILE_SEPARATOR, File.separator);
    }

    @Test
    public void stripValidation() {
        Assertions.assertNull(strip(null, null));
        Assertions.assertEquals("", strip("", null));
        Assertions.assertEquals("", strip("", ""));
        Assertions.assertEquals("a", strip(" a ", null));
        Assertions.assertEquals("a", strip(" a ", " "));
        Assertions.assertEquals("a", strip(" a ", ""));
        Assertions.assertEquals("a", strip(". a .", "."));
        Assertions.assertEquals("a", strip(". a .", ". "));
    }

    @Test
    public void stripStartValidation() {
        Assertions.assertNull(stripStart(null, null));
        Assertions.assertEquals("", stripStart("", null));
        Assertions.assertEquals("", stripStart("", ""));
        Assertions.assertEquals("", stripStart("", " "));
        Assertions.assertEquals("a", stripStart(" a", null));
        Assertions.assertEquals("a", stripStart(" a", " "));
        Assertions.assertEquals("a", stripStart(" a", ""));
        Assertions.assertEquals("a", stripStart(". a", "."));
        Assertions.assertEquals("a", stripStart(". a", ". "));
    }

    @Test
    public void stripEndValidation() {
        Assertions.assertNull(stripEnd(null, null));
        Assertions.assertEquals("", stripEnd("", null));
        Assertions.assertEquals("", stripEnd("", ""));
        Assertions.assertEquals("", stripEnd("", " "));
        Assertions.assertEquals("a", stripEnd("a ", null));
        Assertions.assertEquals("a", stripEnd("a ", " "));
        Assertions.assertEquals("a", stripEnd("a ", ""));
        Assertions.assertEquals("a", stripEnd("a .", "."));
        Assertions.assertEquals("a", stripEnd("a .", ". "));
    }

    @Test
    public void isNumericValidation() {
        Assertions.assertTrue(isNumeric("0"));
        Assertions.assertTrue(isNumeric("0.0"));
        Assertions.assertTrue(isNumeric("-1"));
        Assertions.assertTrue(isNumeric("-1.1"));
        Assertions.assertTrue(isNumeric("-1.1"));
        Assertions.assertFalse(isNumeric(null));
        Assertions.assertFalse(isNumeric(""));
        Assertions.assertFalse(isNumeric(" "));
        Assertions.assertFalse(isNumeric("a"));
        Assertions.assertFalse(isNumeric(" 0"));
        Assertions.assertFalse(isNumeric("0."));
        Assertions.assertFalse(isNumeric(" -1"));
    }

    @Test
    public void isNotNumericValidation() {
        Assertions.assertTrue(isNotNumeric("a"));
        Assertions.assertTrue(isNotNumeric("."));
        Assertions.assertTrue(isNotNumeric("-"));
        Assertions.assertTrue(isNotNumeric(null));
        Assertions.assertTrue(isNotNumeric(""));
        Assertions.assertTrue(isNotNumeric(" "));
        Assertions.assertTrue(isNotNumeric("0."));
        Assertions.assertFalse(isNotNumeric("0"));
        Assertions.assertFalse(isNotNumeric("0.0"));
    }

    @Test
    public void getOrDefaultValidation() {
        Assertions.assertEquals("a", getOrDefault("a", "b"));
        Assertions.assertEquals("b", getOrDefault(null, "b"));
    }

    @Test
    public void avoidFirstZeroValidation() {
        Assertions.assertNull(avoidFirstZero(null, "1"));
        Assertions.assertEquals("", avoidFirstZero("", "1"));
        Assertions.assertEquals("a", avoidFirstZero("a", "1"));
        Assertions.assertEquals("1", avoidFirstZero("0", "1"));
        Assertions.assertEquals("10", avoidFirstZero("00", "1"));
        Assertions.assertEquals("100", avoidFirstZero("000", "1"));
        Assertions.assertEquals("1.5", avoidFirstZero("0.5", "1"));
        Assertions.assertEquals("-1", avoidFirstZero("-1", "1"));
        Assertions.assertEquals("-0.5", avoidFirstZero("-0.5", "1"));
    }

    @Test
    public void getDefaultFieldIndexValidation() {
        Assertions.assertNull(getDefaultFieldIndex(null));
        Assertions.assertEquals("field-default-index", getDefaultFieldIndex("field"));
        Assertions.assertEquals("5-default-index", getDefaultFieldIndex("5"));
    }

    @Test
    public void nextStringValidation() {
        String generated = nextString(1000);

        Assertions.assertEquals(1000, generated.length());

        for(char c : generated.toCharArray()) {
            Assertions.assertTrue(ALPHA_NUMERIC_STRING.contains(Character.toString(c)));
            Assertions.assertFalse(SYMBOLS.contains(Character.toString(c)));
        }
    }

    @Test
    public void nextNumberValidation() {
        String generated = nextNumber(1000);

        Assertions.assertEquals(1000, generated.length());

        for(char c : generated.toCharArray()) {
            Assertions.assertTrue(NUMERIC_STRING.contains(Character.toString(c)));
            Assertions.assertFalse(SYMBOLS.contains(Character.toString(c)));
        }
    }

    @Test
    public void nextStringDifferentSource() {
        String generated = nextString(1000, SYMBOLS.length(),
            (i, index) -> SYMBOLS.charAt(index));

        Assertions.assertEquals(1000, generated.length());

        for(char c : generated.toCharArray()) {
            Assertions.assertTrue(SYMBOLS.contains(Character.toString(c)));
            Assertions.assertFalse(ALPHA_NUMERIC_STRING.contains(Character.toString(c)));
            Assertions.assertFalse(NUMERIC_STRING.contains(Character.toString(c)));
        }
    }

    @Test
    public void nextIndexValidation() {
        for(int i = 0; i < 1000; i++) {
            int generated = nextIndex(i);

            Assertions.assertTrue(generated >= 0);
            Assertions.assertTrue(i == 0 || generated < i);
        }
    }

    @ParameterizedTest
    @MethodSource("toIndexValidationArgs")
    public void toIndexValidation(int length, int number, int expected) {
        Assertions.assertEquals(expected, toIndex(length, number));
    }

    private static Stream<Arguments> toIndexValidationArgs() {
        return Stream.of(
            Arguments.of(5, -10, 0),
            Arguments.of(5, -9, 1),
            Arguments.of(5, -8, 2),
            Arguments.of(5, -7, 3),
            Arguments.of(5, -6, 4),
            Arguments.of(5, -5, 0),
            Arguments.of(5, -4, 1),
            Arguments.of(5, -3, 2),
            Arguments.of(5, -2, 3),
            Arguments.of(5, -1, 4),
            Arguments.of(5, 0, 0),
            Arguments.of(5, 1, 1),
            Arguments.of(5, 2, 2),
            Arguments.of(5, 3, 3),
            Arguments.of(5, 4, 4),
            Arguments.of(5, 5, 0),
            Arguments.of(5, 6, 1),
            Arguments.of(5, 7, 2),
            Arguments.of(5, 8, 3),
            Arguments.of(5, 9, 4),
            Arguments.of(5, 10, 0)
        );
    }

    @Test
    public void getAllFieldsValidation() {
        Map<String, Field> fields = getAllFields(Pair.class);

        Assertions.assertFalse(fields.isEmpty());
        Assertions.assertTrue(fields.containsKey("left"));
        Assertions.assertTrue(fields.containsKey("right"));
    }

    @Test
    public void getAllFieldsInvalidObject() {
        Map<String, Field> nullFields = getAllFields(null);

        Assertions.assertTrue(nullFields.isEmpty());

        Map<String, Field> objectFields = getAllFields(Object.class);

        Assertions.assertTrue(objectFields.isEmpty());
    }

    @Test
    public void formatAsOrderedListValidation() {
        List<String> entries = Arrays.asList("Entry 1", "Entry 2", "Entry 3");
        List<String> formattedEntries = formatAsOrderedList(entries,
            (i, entry) -> entry.equalsIgnoreCase("Entry 2"),
            entry -> entry);

        Assertions.assertEquals("1) Entry 1", formattedEntries.get(0));
        Assertions.assertEquals("2) (default) Entry 2", formattedEntries.get(1));
        Assertions.assertEquals("3) Entry 3", formattedEntries.get(2));
    }

    @Test
    public void formatInColumnsValidation() {
        Settings settings = new Settings();
        List<String> entries = Arrays.asList("Entry 1", "Entry 2", "Entry 3");
        String output = formatInColumns(settings, entries).toString();

        for(String entry : entries) {
            Assertions.assertTrue(output.contains(entry));
        }

        Assertions.assertTrue(output.endsWith("\n"));
    }

    @Test
    public void formatInColumnsMaxLength() {
        Settings settings = new Settings();
        List<String> entries = Arrays.asList(
            String.format("%-" + settings.getConsoleWidth() + "s", "Entry 1"),
            String.format("%-" + settings.getConsoleWidth() + "s", "Entry 2"),
            String.format("%-" + settings.getConsoleWidth() + "s", "Entry 3")
        );
        String output = formatInColumns(settings, entries).toString();
        String[] lines = output.split("\n");

        Assertions.assertEquals(3, lines.length);
        Assertions.assertTrue(lines[0].contains("Entry 1"));
        Assertions.assertTrue(lines[1].contains("Entry 2"));
        Assertions.assertTrue(lines[2].contains("Entry 3"));
    }

    @Test
    public void formatInColumnsSingleLines() {
        Settings settings = new Settings();
        List<String> entries = Arrays.asList("Entry 1", "Entry 2");
        int columnLength = settings.getConsoleWidth() / (entries.size() + 1);
        
        entries = entries
            .stream()
            .map(e -> String.format("%-" + columnLength + "s", e))
            .collect(Collectors.toList());

        String output = formatInColumns(settings, entries).toString();
        String[] lines = output.split("\n");

        Assertions.assertEquals(1, lines.length);
        Assertions.assertTrue(lines[0].contains("Entry 1"));
        Assertions.assertTrue(lines[0].contains("Entry 2"));
    }

    @ParameterizedTest
    @MethodSource("selectActiveValidationArgs")
    public void selectActiveValidation(String groupName, String active, int expectedIndex) {
        GroupConfig groupNegative = createGroupConfig(active, 3, "a", "b", "c");
        Map<String, String> propsIndexNegative = Assertions.assertDoesNotThrow(
            () -> selectActive(Optional.empty(), groupName, groupNegative));

        Assertions.assertTrue(propsIndexNegative.keySet()
            .stream()
            .allMatch(k -> k.startsWith(groupName + "." + expectedIndex + "-")));
    }

    private static Stream<Arguments> selectActiveValidationArgs() {
        return Stream.of(
            Arguments.of("index-negative", "-1", 2),
            Arguments.of("index-0", "0", 0),
            Arguments.of("index-1", "1", 1),
            Arguments.of("index-2", "2", 2),
            Arguments.of("index-exceeded", "3", 0),
            Arguments.of("index-first", "FIRST", 0),
            Arguments.of("index-lasr", "LAST", 2)
        );
    }

    @Test
    public void selectActiveIndexEmpty() {
        GroupConfig emptyGroup = createGroupConfig("0", 0);
        Map<String, String> emptyProps = Assertions.assertDoesNotThrow(
            () -> selectActive(Optional.empty(), "empty", emptyGroup));

        Assertions.assertNotNull(emptyProps);
        Assertions.assertTrue(emptyProps.isEmpty());
    }

    @Test
    public void selectActiveRandom() {
        GroupConfig group = createGroupConfig("ANY", 3, "a", "b", "c");
        Map<String, String> props = Assertions.assertDoesNotThrow(
            () -> selectActive(Optional.empty(), "random", group));

        Pattern pattern = Pattern.compile("random\\.\\d-");
        Assertions.assertTrue(props.keySet()
            .stream()
            .map(k -> pattern.matcher(k))
            .allMatch(Matcher::find));

        GroupConfig emptyGroup = createGroupConfig("ANY", 0);
        Map<String, String> emptyProps = Assertions.assertDoesNotThrow(
            () -> selectActive(Optional.empty(), "empty", emptyGroup));

        Assertions.assertNotNull(emptyProps);
        Assertions.assertTrue(emptyProps.isEmpty());
    }

    @Test
    public void selectActiveConfig() {
        GroupConfig emptyGroup = createGroupConfig("FIRST", 3, "a", "b", "c");
        Optional<Settings> settings = Optional.of(new Settings());
        String groupParam = toGroupParam(settings, "config");

        settings.get().putOverride(groupParam, "LAST");

        Map<String, String> properties = Assertions.assertDoesNotThrow(
            () -> selectActive(settings, "config", emptyGroup));

        Assertions.assertNotNull(properties);
        Assertions.assertEquals("2-a", properties.get("config.2-a"));
        Assertions.assertEquals("2-b", properties.get("config.2-b"));
        Assertions.assertEquals("2-c", properties.get("config.2-c"));
    }

    @Test
    public void selectActiveInvalid() {
        GroupConfig group = createGroupConfig("INVALID", 3, "a", "b", "c");
        
        Assertions.assertThrows(IllegalStateException.class,
            () -> selectActive(Optional.empty(), "invalid", group));
    }

    private GroupConfig createGroupConfig(String active, int collections, String... properties) {
        GroupConfig group = new GroupConfig();
        group.setActive(active);
        group.setCollection(new ArrayList<>());

        for(int i = 0; i < collections; i++) {
            Map<String, String> props = new HashMap<>();

            for(String property : properties) {
                String value = i + "-" + property;
                props.put(value, value);
            }

            group.getCollection().add(props);
        }

        return group;
    }
}
