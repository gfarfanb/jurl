package com.legadi.jurl.executor.mixer.adapter;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.legadi.jurl.exception.InvalidInputEntryException;
import com.legadi.jurl.executor.mixer.JsonBodyMixer.ListRule;
import com.legadi.jurl.executor.mixer.JsonBodyMixer.RuleEntry;

public class RuleEntryDeserializer implements JsonDeserializer<RuleEntry> {

    private static final String FIELD_LIST_MERGE_RULE = "listMergeRule";
    private static final String FIELD_KEY_FIELDS = "keyFields";

    @Override
    public RuleEntry deserialize(JsonElement json, Type type,
            JsonDeserializationContext context) throws JsonParseException {
        Set<String> ruleFields = Arrays.stream(RuleEntry.class.getDeclaredFields())
                .map(Field::getName)
                .collect(Collectors.toSet());
        JsonObject object = (JsonObject) json;

        if(object.isEmpty()) {
            throw new JsonParseException("No fields defined for: " + RuleEntry.class);
        }

        for(String field : object.keySet()) {
            if(!ruleFields.contains(field)) {
                throw new JsonParseException("Field [" + field + "] is not for: " + RuleEntry.class);
            }
        }

        if(!object.has(FIELD_LIST_MERGE_RULE)) {
            throw new InvalidInputEntryException("Required field [" + FIELD_LIST_MERGE_RULE + "] for: " + RuleEntry.class);
        }

        RuleEntry ruleEntry = new RuleEntry();

        try {
            ruleEntry.setListMergeRule(ListRule.valueOf(object.get(FIELD_LIST_MERGE_RULE).getAsString()));
        } catch(IllegalArgumentException ex) {
            throw new InvalidInputEntryException("Unknown enum: [" + FIELD_LIST_MERGE_RULE + "] for: " + ListRule.class);
        }

        if(object.get(FIELD_KEY_FIELDS) != null) {
            ruleEntry.setKeyFields(
                object.get(FIELD_KEY_FIELDS)
                    .getAsJsonArray()
                    .asList()
                    .stream()
                    .map(JsonElement::getAsString)
                    .collect(Collectors.toSet())
            );
        }

        return ruleEntry;
    }

}
