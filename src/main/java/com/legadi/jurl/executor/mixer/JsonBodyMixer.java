package com.legadi.jurl.executor.mixer;

import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.JsonUtils.isArrayFile;
import static com.legadi.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.jurl.common.JsonUtils.toJsonString;
import static com.legadi.jurl.common.JsonUtils.writeJsonFile;
import static com.legadi.jurl.common.WriterUtils.expandFile;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.executor.mixer.adapter.RuleEntryDeserializer;

public class JsonBodyMixer implements BodyMixer {

    public enum ListRule {

        REPLACE, ADD_BEGIN, ADD_END, MERGE
    }

    @Override
    public boolean accepts(String bodyType) {
        return "json".equalsIgnoreCase(bodyType);
    }

    @Override
    public Path apply(Settings settings, MixerEntry entry) {
        Path temporalBodyPath = expandBodyFile(settings, entry);
        Object json;

        if(isArrayFile(temporalBodyPath)) {
            List<Object> list = jsonToObject(entry.getBodyContent(), new TypeToken<List<Object>>() {});
            RuleEntry ruleEntry = extractRuleEntry(list);

            json = mergeList(
                ruleEntry,
                loadJsonFile(temporalBodyPath.toString(), new TypeToken<List<Object>>() {}),
                list
            );
        } else {
            json = mergeObject(
                loadJsonFile(temporalBodyPath.toString(), new TypeToken<Map<String, Object>>() {}),
                jsonToObject(entry.getBodyContent(), new TypeToken<Map<String, Object>>() {})
            );
        }

        writeJsonFile(temporalBodyPath, json);

        return temporalBodyPath;
    }

    @SuppressWarnings("unchecked")
    private Object mergeObject(Map<String, Object> base, Map<String, Object> update) {
        for(Map.Entry<String, Object> updateEntry : update.entrySet()) {
            if(updateEntry.getValue() instanceof Map) {
                base.put(updateEntry.getKey(), mergeObject(
                    (Map<String, Object>) base.get(updateEntry.getKey()),
                    (Map<String, Object>) updateEntry.getValue() 
                ));
            } else if(updateEntry.getValue() instanceof List) {
                List<Object> list = (List<Object>) updateEntry.getValue();
                RuleEntry ruleEntry = extractRuleEntry(list);

                base.put(updateEntry.getKey(), mergeList(
                    ruleEntry, (List<Object>) base.get(updateEntry.getKey()), list
                ));
            } else {
                base.put(updateEntry.getKey(), updateEntry.getValue());
            }
        }
        return base;
    }

    private Object mergeList(RuleEntry ruleEntry, List<Object> base, List<Object> update) {
        switch(ruleEntry.getListMergeRule()) {
            case ADD_BEGIN:
                update.addAll(base);
                return update;
            case ADD_END:
                base.addAll(update);
                return base;
            case MERGE:
                return mergeList(ruleEntry.getKeyFields(), base, update);
            default:
                return update;
        }
    }

    @SuppressWarnings("unchecked")
    private Object mergeList(Set<String> keyFields, List<Object> base, List<Object> update) {
        if(isEmpty(keyFields)) {
            base.addAll(update);
            return base;
        }

        for(Object value : update) {
            if(!(value instanceof Map)) {
                base.add(value);
                continue;
            }
            
            Map<String, Object> updateObject = (Map<String, Object>) value;
            Predicate<Map<String, Object>> targetPredicate = target ->
                keyFields
                    .stream()
                    .allMatch(key -> Objects.equals(updateObject.get(key), target.get(key)));
            Optional<Map<String, Object>> targetObject = base
                .stream()
                .map(object -> (Map<String, Object>) object)
                .filter(targetPredicate)
                .findFirst();

            if(targetObject.isPresent()) {
                mergeObject(targetObject.get(), updateObject);
            }
        }

        return base;
    }

    private RuleEntry extractRuleEntry(List<Object> list) {
        if(isEmpty(list)) {
            return new RuleEntry(ListRule.REPLACE);
        }

        try {
            String ruleElementRaw = toJsonString(list.get(0));
            RuleEntry ruleEntry = jsonToObject(ruleElementRaw, new TypeToken<RuleEntry>() {});
            list.remove(0);
            return ruleEntry;
        } catch(IllegalStateException ex) {
            return new RuleEntry(ListRule.REPLACE);
        }
    }

    private Path expandBodyFile(Settings settings, MixerEntry entry) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(entry.getRequestPath())
            .setRequestName(entry.getRequestName())
            .setExtension("body");
        Path temporalBodyPath = pathBuilder.buildCommandPath();

        expandFile(settings, entry.getBodyFilePath(), temporalBodyPath);

        return temporalBodyPath;
    }

    @JsonAdapter(RuleEntryDeserializer.class)
    public static class RuleEntry {

        private ListRule listMergeRule;
        private Set<String> keyFields;

        public RuleEntry() {}

        private RuleEntry(ListRule listMergeRule) {
            this.listMergeRule = listMergeRule;
        }

        public ListRule getListMergeRule() {
            return listMergeRule;
        }

        public void setListMergeRule(ListRule listMergeRule) {
            this.listMergeRule = listMergeRule;
        }

        public Set<String> getKeyFields() {
            return keyFields;
        }

        public void setKeyFields(Set<String> keyFields) {
            this.keyFields = keyFields;
        }
    }
}
