package com.legadi.jurl.model.adapter;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;

import com.legadi.jurl.common.StringUtils;

public class FlowsDeserializer implements JsonDeserializer<Map<String, String[]>> {

    @Override
    public Map<String, String[]> deserialize(JsonElement json, Type type,
            JsonDeserializationContext context) throws JsonParseException {
        return json.getAsJsonObject().entrySet()
            .stream()
            .collect(Collectors.toMap(
                e -> e.getKey(),
                e -> toArray(e.getValue()),
                (v1, v2) -> v1,
                HashMap::new
            ));
    }

    private String[] toArray(JsonElement json) {
        return json.getAsJsonArray().asList()
            .stream()
            .map(StringUtils::toJsonString)
            .toArray(String[]::new);
    }
}
