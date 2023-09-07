package com.legadi.jurl.executor.reader;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Consumer;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;

import static com.legadi.jurl.common.StringUtils.isNotBlank;

public class JsonOutputReader implements OutputReader<Map<String, String>> {

    private final Random random = new Random();

    @Override
    public Map<String, String> apply(Path sourcePath, Set<String> params, String paramPrefix) {
        Map<String, String> output = new HashMap<>();

        params = normalizeParams(params);

        try (JsonReader jsonReader = new JsonReader(Files.newBufferedReader(sourcePath))) {
            Deque<Element> elements = new LinkedList<>();
            boolean complete = false;

            while (!complete && jsonReader.hasNext()) {
                JsonToken nextToken = jsonReader.peek();
                String value = null;

                switch (nextToken) {
                    case BEGIN_OBJECT:
                        jsonReader.beginObject();
                        elements.peekLast().incrementArrayCount();
                        elements.add(new Element(false));
                        break;
                    case BEGIN_ARRAY:
                        jsonReader.beginArray();
                        elements.add(new Element(true));
                        break;
                    case NAME:
                        Element element = elements.peekLast();
                        element.setName(jsonReader.nextName());
                        break;
                    case BOOLEAN:
                        elements.peekLast().incrementArrayCount();
                        value = Boolean.toString(jsonReader.nextBoolean());
                        setValue(paramPrefix, elements, params, output, value);
                        break;
                    case NUMBER:
                    case STRING:
                        elements.peekLast().incrementArrayCount();
                        value = jsonReader.nextString();
                        setValue(paramPrefix, elements, params, output, value);
                        break;
                    case NULL:
                        elements.peekLast().incrementArrayCount();
                        jsonReader.nextNull();
                        setValue(paramPrefix, elements, params, output, value);
                        break;
                    case END_ARRAY:
                        jsonReader.endArray();
                        setArraySize(paramPrefix, elements, params, output);
                        elements.removeLast();
                        break;
                    case END_OBJECT:
                        jsonReader.endObject();
                        elements.removeLast();
                        break;
                    case END_DOCUMENT:
                        complete = true;
                        break;
                }
            }
        } catch (IOException ex) {
            throw new IllegalStateException("Unable to read JSON file: " + sourcePath, ex);
        }

        return output;
    }

    private Set<String> normalizeParams(Set<String> params) {
        Set<String> normalizedParams = new HashSet<>();

        for(String param : params) {
            String[] parts = param.split(".");
            StringBuilder paramParts = new StringBuilder();
            boolean isFirst = true;

            for(String part : parts) {
                if(isFirst) {
                    paramParts.append(part);
                    normalizedParams.add(part);
                    isFirst = false;
                } else {
                    paramParts.append('.').append(part);
                    normalizedParams.add(paramParts.toString());
                }
            }

            normalizedParams.add(param);
        }

        return normalizedParams;
    }

    private void setArraySize(String paramPrefix, Deque<Element> elements, Set<String> params,
            Map<String, String> output) {
        String size = Integer.toString(elements.peekLast().getArrayCount());

        elements.add(new Element(false, "_size_"));
        setValue(paramPrefix, elements, params, output, size);
        elements.removeLast();
    }

    private void setValue(String paramPrefix, Deque<Element> elements, Set<String> params,
            Map<String, String> output, String value) {
        Consumer<String> setter = param -> output.put(param, value);
        String lastElement;
        String param;

        // [first]
        // []
        // [any]
        // [last]
        // [n]
    }

    private String buildParamPart(String prefix, Iterable<String> parts) {
        StringBuilder paramBuilder = new StringBuilder();

        if (isNotBlank(prefix)) {
            paramBuilder.append(prefix);
        }

        for (String part : parts) {
            paramBuilder.append(part).append('.');
        }

        return paramBuilder.toString();
    }

    public static class Element {

        private final boolean array;

        private String name;
        private int arrayCount = -1;

        public Element(boolean array) {
            this(array, null);
        }

        public Element(boolean array, String name) {
            this.array = array;
            this.name = name;
        }

        public boolean isArray() {
            return array;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public int getArrayCount() {
            return arrayCount;
        }

        public void incrementArrayCount() {
            if(array) {
                arrayCount++;
            }
        }
    }
}
