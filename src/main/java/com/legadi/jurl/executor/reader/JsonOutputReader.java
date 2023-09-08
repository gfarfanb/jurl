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
                        setValue(paramPrefix, elements, params, output,
                            Boolean.toString(jsonReader.nextBoolean()));
                        break;
                    case NUMBER:
                    case STRING:
                        elements.peekLast().incrementArrayCount();
                        setValue(paramPrefix, elements, params, output,
                            jsonReader.nextString());
                        break;
                    case NULL:
                        elements.peekLast().incrementArrayCount();
                        jsonReader.nextNull();
                        setValue(paramPrefix, elements, params, output, "");
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
        Element[] elementArray = elements.toArray(new Element[elements.size()]);
        StringBuilder paramBuilder = new StringBuilder(paramPrefix);

        setParamValue(paramBuilder, elementArray, setter);
    }

    private void setParamValue(StringBuilder paramBuilder, Element[] elements, Consumer<String> setter) {
        int index = -1;

        for(Element element : elements) {
            index++;

            if(!element.isArray()) {
                paramBuilder.append(element.getName()).append('.');
                continue;
            }

            String paramPart = paramBuilder.toString() +
                (isNotBlank(element.getName()) ? element.getName() : "");

            if(index == elements.length - 1) {
                setter.accept(paramPart + "[" + index + "]");

                if(element.getArrayCount() == 0) {
                    setter.accept(paramPart + "[first]");
                    setter.accept(paramPart + "[]");
                    setter.accept(paramPart + "[any]");
                } else if(element.getArrayAny() == 0 && random.nextBoolean()) {
                    element.setArrayAny(element.getArrayCount());
                    setter.accept(paramPart + "[]");
                    setter.accept(paramPart + "[any]");
                } else if(element.getArrayAny() == element.getArrayCount()) {
                    setter.accept(paramPart + "[]");
                    setter.accept(paramPart + "[any]");
                }

                setter.accept(paramPart + "[last]");

                return;
            } else {
                Element[] next = new Element[elements.length - index - 1];
                System.arraycopy(elements, index + 1, next, 0, next.length);

                setParamValue(new StringBuilder(paramPart + "[" + index + "]").append('.'), next, setter);

                if(element.getArrayCount() == 0) {
                    setParamValue(new StringBuilder(paramPart + "[first]").append('.'), next, setter);
                    setParamValue(new StringBuilder(paramPart + "[]").append('.'), next, setter);
                    setParamValue(new StringBuilder(paramPart + "[any]").append('.'), next, setter);
                } else if(element.getArrayAny() == 0 && random.nextBoolean()) {
                    element.setArrayAny(element.getArrayCount());
                    setParamValue(new StringBuilder(paramPart + "[]").append('.'), next, setter);
                    setParamValue(new StringBuilder(paramPart + "[any]").append('.'), next, setter);
                } else if(element.getArrayAny() == element.getArrayCount()) {
                    setParamValue(new StringBuilder(paramPart + "[]").append('.'), next, setter);
                    setParamValue(new StringBuilder(paramPart + "[any]").append('.'), next, setter);
                }

                setParamValue(new StringBuilder(paramPart + "[last]").append('.'), next, setter);
            }
        }

        paramBuilder.deleteCharAt(paramBuilder.length() - 1);
        setter.accept(paramBuilder.toString());
    }

    public static class Element {

        private final boolean array;

        private String name;
        private int arrayAny = 0;
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

        public int getArrayAny() {
            return arrayAny;
        }

        public void setArrayAny(int arrayAny) {
            this.arrayAny = arrayAny;
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
