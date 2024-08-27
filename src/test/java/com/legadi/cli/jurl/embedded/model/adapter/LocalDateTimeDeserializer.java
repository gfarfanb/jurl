package com.legadi.cli.jurl.embedded.model.adapter;

import java.lang.reflect.Type;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;

public class LocalDateTimeDeserializer implements JsonDeserializer<LocalDateTime> {

    @Override
    public LocalDateTime deserialize(JsonElement json, Type type,
            JsonDeserializationContext context) throws JsonParseException {
        return LocalDateTime.from(
            DateTimeFormatter.ISO_LOCAL_DATE_TIME.parse(json.getAsString())
        );
    }
    
}
