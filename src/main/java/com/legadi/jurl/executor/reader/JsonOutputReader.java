package com.legadi.jurl.executor.reader;

import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNumeric;
import static com.legadi.jurl.common.CommonUtils.nextIndex;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.JsonUtils.isArrayFile;
import static com.legadi.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.exception.CommandException;

public class JsonOutputReader implements OutputReader {

    private static final Pattern ARRAY_CALL = Pattern.compile("(.*)(\\[.*\\])");
    private static final String LIST_SIZE = "__size__";
    private static final String LIST_IN_OBJECT_SIZE = "/__size__";
    private static final int FIRST_ELEMENT = 0;

    private final Map<String, Integer> anyIndexes = new HashMap<>();

    @Override
    public boolean accepts(String contentType) {
        return accepts(contentType, "application/json", "application/ld+json");
    }

    @Override
    public boolean isPrintable() {
        return true;
    }

    @Override
    public Map<String, String> apply(Path sourcePath, Path outputPath, Set<String> params, String paramPrefix) {
        Map<String, String> output = new HashMap<>();
        Object jsonContent = isArrayFile(sourcePath)
            ? loadJsonFile(sourcePath.toString(), new TypeToken<List<Object>>() {})
            : loadJsonFile(sourcePath.toString(), new TypeToken<Map<String, Object>>() {});

        for(String param : params) {
            String elementCall = param.substring(paramPrefix.length(), param.length());
            String[] callParts = elementCall.split("\\.");
            Object value = getValue(FIRST_ELEMENT, callParts, jsonContent, "");

            if(value instanceof List || value instanceof Map) {
                writeJsonFile(outputPath.resolve(elementCall), value);
                output.put(param, outputPath.resolve(elementCall).toString());
            } else if(value == null) {
                output.put(param, null);
            } else {
                output.put(param, value.toString());
            }
        }

        return output;
    }

    private Object getValue(int partIndex, String[] callParts, Object jsonRaw, String partsKey) {
        if(partIndex >= callParts.length) {
            return jsonRaw;
        }

        Matcher arrayMatcher = ARRAY_CALL.matcher(callParts[partIndex]);
        String field = callParts[partIndex];
        String element = "";

        if(arrayMatcher.find()) {
            field = arrayMatcher.group(1);
            element = arrayMatcher.group(2);
        }

        partsKey += isNotBlank(field) ? "." + field : "";

        if(field.endsWith(LIST_SIZE)) {
            return getListSize(field, callParts, jsonRaw);
        } else if(jsonRaw instanceof List) {
            return getListValue(partIndex, element, callParts, jsonRaw, partsKey);
        } else if(jsonRaw instanceof Map) {
           return getMapValue(partIndex, field, element, callParts, jsonRaw, partsKey);
        } else {
            throw new CommandException("Param " + String.join(".", callParts) + " doesn't match with JSON response");
        }
    }

    @SuppressWarnings("unchecked")
    private int getListSize(String field, String[] callParts, Object jsonRaw) {
        if(jsonRaw instanceof List) {
            List<Object> jsonList = (List<Object>) jsonRaw;

            if(!field.equals(LIST_SIZE)) {
                throw new CommandException("Size from field must follow this sintax '__size__': " + field);
            }

            return jsonList.size();
        } else if(jsonRaw instanceof Map) {
            Map<String, Object> jsonObject = (Map<String, Object>) jsonRaw;

            if(!field.endsWith(LIST_IN_OBJECT_SIZE)) {
                throw new CommandException("Size from field must follow this sintax '<field>/__size__': " + field);
            }

            field = field.substring(0, field.length() - LIST_IN_OBJECT_SIZE.length());

            Object objectValue = jsonObject.get(field);

            if(objectValue instanceof List) {
                return ((List<Object>) objectValue).size();
            } else {
                throw new CommandException("Field is not type list: " + field);
            }
        } else {
            throw new CommandException("Param " + String.join(".", callParts) + " doesn't match with JSON response");
        }
    }

    @SuppressWarnings("unchecked")
    private Object getListValue(int partIndex, String element, String[] callParts, Object jsonRaw, String partsKey) {
        List<Object> jsonList = (List<Object>) jsonRaw;
        String indexRaw = strip(element, "[]");
        Object listValue = getListValueByIndex(indexRaw, jsonList, partsKey);
        return getValue(partIndex + 1, callParts, listValue, partsKey);
    }

    @SuppressWarnings("unchecked")
    private Object getMapValue(int partIndex, String field, String element, String[] callParts,
            Object jsonRaw, String partsKey) {
         Map<String, Object> jsonObject = (Map<String, Object>) jsonRaw;
        String indexRaw = strip(element, "[]");
        Object objectValue = jsonObject.get(field);

        if(objectValue instanceof List && isNotBlank(element)) {
            objectValue = getListValueByIndex(indexRaw, (List<Object>) objectValue, partsKey);
        }

        return getValue(partIndex + 1, callParts, objectValue, partsKey);
    }

    private Object getListValueByIndex(String indexRaw, List<Object> jsonList, String partsKey) {
        if(isEmpty(jsonList)) {
            return null;
        }

        if("first".equalsIgnoreCase(indexRaw)) {
            return jsonList.get(FIRST_ELEMENT);
        }

        if("last".equalsIgnoreCase(indexRaw)) {
            return jsonList.get(jsonList.size() - 1);
        }

        if(isNumeric(indexRaw)) {
            try {
                return jsonList.get(Integer.parseInt(indexRaw));
            } catch(IndexOutOfBoundsException ex) {
                return null;
            }
        }

        if(isBlank(indexRaw) || "any".equalsIgnoreCase(indexRaw)) {
            int anyIndex = anyIndexes.getOrDefault(partsKey, nextIndex(jsonList.size()));
            anyIndexes.put(partsKey, anyIndex);
            return jsonList.get(anyIndex);
        }

        throw new CommandException("Invalid JSON array index: " + indexRaw);
    }
}
