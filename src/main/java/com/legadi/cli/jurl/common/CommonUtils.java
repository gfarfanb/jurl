package com.legadi.cli.jurl.common;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class CommonUtils {

    private static final Pattern NUMBER_PATTERN = Pattern.compile("-?\\d+(\\.\\d+)?");
    private static final String DEFAULT_FIELD_INDEX_FORMAT = "%s-default-index";

    public static final String ALPHA_NUMERIC_STRING = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvxyz0123456789";
    public static final String NUMERIC_STRING = "0123456789";

    public static final String REQUEST_TAG = "[rq]";
    public static final String REQUEST_NAME = "request";
    public static final String FLOW_TAG = "[fl]";
    public static final String FLOW_NAME = "flow";

    public static final int TABS_FOR_MARGIN = 2;
    public static final int START_INDEX = 1;
    public static final int INVALID_INDEX = -1;

    public static final Map<String, String> EMPTY_MAP = new HashMap<>();

    private CommonUtils() {}

    public static boolean isNotBlank(String value) {
        return value != null && !value.trim().isEmpty();
    }

    public static boolean isBlank(String value) {
        return !isNotBlank(value);
    }

    public static boolean isNotEmpty(Collection<?> collection) {
        return collection != null && !collection.isEmpty();
    }

    public static boolean isEmpty(Collection<?> collection) {
        return !isNotEmpty(collection);
    }

    public static boolean isNotEmpty(Map<?, ?> map) {
        return map != null && !map.isEmpty();
    }

    public static boolean isEmpty(Map<?, ?> map) {
        return !isNotEmpty(map);
    }

    public static boolean isNotEmpty(Object[] array) {
        return array != null && array.length > 0;
    }

    public static boolean isEmpty(Object[] array) {
        return !isNotEmpty(array);
    }

    public static String trim(String value) {
        return strip(value, null);
    }

    public static String fileSeparatorAsDelimiter(Settings settings) {
        String separator = settings.getFileSeparator();
        if(separator.equals("\\")) {
            return "\\\\";
        } else {
            return separator;
        }
    }

    public static String strip(String value, String stripChars) {
        value = stripStart(value, stripChars);
        return stripEnd(value, stripChars);
    }

    public static String stripStart(String value, String stripChars) {
        if(value == null) {
            return value;
        }
        stripChars = stripChars != null && stripChars.isEmpty() ? null : stripChars;
        int index = 0;
        if(stripChars == null) {
            while(index != value.length() && Character.isWhitespace(value.charAt(index))) {
                index++;
            }
        } else {
            while(index != value.length()
                    && (stripChars.indexOf(value.charAt(index)) != -1
                        || Character.isWhitespace(value.charAt(index)))) {
                index++;
            }
        }
        return value.substring(index);
    }

    public static String stripEnd(String value, String stripChars) {
        if(value == null) {
            return value;
        }
        stripChars = stripChars != null && stripChars.isEmpty() ? null : stripChars;
        int length = value.length();
        if(stripChars == null) {
            while(length != 0 && Character.isWhitespace(value.charAt(length - 1))) {
                length--;
            }
        } else {
            while(length != 0
                    && (stripChars.indexOf(value.charAt(length - 1)) != -1
                        || Character.isWhitespace(value.charAt(length - 1)))) {
                length--;
            }
        }
        return value.substring(0, length);
    }

    public static boolean isNotNumeric(String value) {
        return !isNumeric(value);
    }

    public static boolean isNumeric(String value) {
        if (value == null) {
            return false; 
        }
        return NUMBER_PATTERN.matcher(value).matches();
    }

    public static <T> T getOrDefault(T value, T defaultValue) {
        if(value == null) {
            return defaultValue;
        } else {
            return value;
        }
    }

    public static String avoidFirstZero(String number, String zeroReplacer) {
        if(!isNumeric(number)) {
            return number;
        }
        char firstChar = number.charAt(0);
        if(firstChar == '0') {
            if(number.length() == 1) {
                return zeroReplacer;
            } else {
                return zeroReplacer + number.substring(1);
            }
        } else {
            return number;
        }
    }

    public static String getDefaultFieldIndex(String field) {
        if(field == null) {
            return null;
        }
        return String.format(DEFAULT_FIELD_INDEX_FORMAT, field);
    }

    public static String nextString(int length) {
        return nextString(length, ALPHA_NUMERIC_STRING.length(),
            (i, randomIndex) -> ALPHA_NUMERIC_STRING.charAt(randomIndex));
    }

    public static String nextNumber(int length) {
        return nextString(length, NUMERIC_STRING.length(),
            (i, randomIndex) -> NUMERIC_STRING.charAt(randomIndex));
    }

    public static <T> String nextString(int length, int sourceLength,
            BiFunction<Integer, Integer, T> indexMapper) {
        StringBuilder elements = new StringBuilder();
        int randomIndex;

        for (int i = 0; i < length; i++) {
            randomIndex = nextIndex(sourceLength);
            elements.append(indexMapper.apply(i, randomIndex));
        }

        return elements.toString();
    }

    public static int nextIndex(int length) {
        return (int) (length * Math.random());
    }

    public static Map<String, Field> getAllFields(Class<?> type) {
        Map<String, Field> fields = new HashMap<>();

        while (type != null && type != Object.class) {
            Arrays.stream(type.getDeclaredFields())
                .forEach(f -> fields.put(f.getName(), f));

            type = type.getSuperclass();
        }

        return fields;
    }



    public static List<String> formatAsOrderedList(List<String> entries,
            BiPredicate<Integer, String> isDefault,
            Function<String, String> decorator) {
        List<String> formattedEntries = new ArrayList<>();
        int index = START_INDEX;

        for(String entry : entries) {
            formattedEntries.add(
                index + ") "
                + (isDefault.test(index, entry) ? "(default) " : "")
                + decorator.apply(entry)
            );
            index++;
        }

        return formattedEntries;
    }

    public static StringBuilder formatInColumns(Settings settings, List<String> entries) {
        entries = entries.stream().map(e -> e + settings.getTab()).collect(Collectors.toList());

        StringBuilder formatted = new StringBuilder();
        int maxLength = entries.stream()
            .mapToInt(String::length)
            .max()
            .orElse(1);
        int columns = (settings.getConsoleWidth() - (settings.getConsoleTabLength() * TABS_FOR_MARGIN))
            / maxLength;

        if(columns > 1) {
            appendInColumns(formatted, entries, columns, maxLength);
        } else {
            entries.forEach(e -> formatted.append(e).append("\n"));
        }

        return formatted;
    }

    private static void appendInColumns(StringBuilder menu, List<String> entries,
            int columns, int maxLength) {
        boolean newLine = false;
        int column = 0;

        for(String entry : entries) {
            menu.append(String.format("%-" + maxLength + "s", entry));

            if(column < (columns - 1)) {
                newLine = false;
                column++;
            } else {
                newLine = true;
                column = 0;
                menu.append("\n");
            }
        }

        if(!newLine) {
            menu.append("\n");
        }
    }
}
