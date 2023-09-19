package com.legadi.jurl.model.adapter;

import static com.legadi.jurl.common.CommonUtils.toJsonString;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;

public class RequestsDeserializer implements JsonDeserializer<Map<String, String>> {

    @Override
    public Map<String, String> deserialize(JsonElement json, Type type,
            JsonDeserializationContext context) throws JsonParseException {
        return json.getAsJsonObject().entrySet()
            .stream()
            .collect(Collectors.toMap(
                e -> e.getKey(),
                e -> toJsonString(e.getValue()),
                (v1, v2) -> v1,
                HashMap::new
            ));
    }
}
