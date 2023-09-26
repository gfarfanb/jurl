package com.legadi.jurl.model.adapter;

import static com.legadi.jurl.common.CommonUtils.toJsonString;

import java.lang.reflect.Type;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;

public class APIDeserializer implements JsonDeserializer<String> {

    @Override
    public String deserialize(JsonElement json, Type type,
            JsonDeserializationContext context) throws JsonParseException {
        return toJsonString(json);
    }
    
}
