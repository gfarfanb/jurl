package com.legadi.cli.jurl.executor.mixer;

import static com.legadi.cli.jurl.common.CommonUtils.isEmpty;
import static com.legadi.cli.jurl.common.JsonUtils.isArrayFile;
import static com.legadi.cli.jurl.common.JsonUtils.jsonToObject;
import static com.legadi.cli.jurl.common.JsonUtils.loadJsonFile;
import static com.legadi.cli.jurl.common.JsonUtils.toJsonString;
import static com.legadi.cli.jurl.common.JsonUtils.writeJsonFile;
import static com.legadi.cli.jurl.common.WriterUtils.expandFile;
import static com.legadi.cli.jurl.common.annotations.Evaluable.Operation.EQUALS_IGNORE_CASE;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.reflect.TypeToken;
import com.legadi.cli.jurl.common.OutputPathBuilder;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Evaluable;
import com.legadi.cli.jurl.common.annotations.Typed;
import com.legadi.cli.jurl.exception.CommandException;
import com.legadi.cli.jurl.executor.mixer.adapter.RuleEntryDeserializer;

@Typed(type = "json")
@Evaluable(values = { "json" }, op = EQUALS_IGNORE_CASE)
public class JsonBodyMixer implements BodyMixer {

    public enum ListRule {

        REPLACE, ADD_BEGIN, ADD_END, MERGE
    }

    @Override
    public Path apply(Settings settings, Map<String, Object> defaults, MixerEntry entry) {
        Path bodyBasePath = expandBodyFile(settings, defaults, entry, "body-base", entry.getBodyBasePath());
        Path bodyComparePath = expandBodyFile(settings, defaults, entry, "body-compare", entry.getBodyComparePath());
        boolean isBaseArray = isArrayFile(bodyBasePath);
        boolean isCompareArray = isArrayFile(bodyComparePath);

        if(isBaseArray != isCompareArray) {
            throw new CommandException("Body base type "
                + "[" + (isBaseArray ? "Array" : "Object") + "]"
                + " is different to body compare type "
                + "[" + (isCompareArray ? "Array" : "Object") + "]");
        }

        Object json;

        if(isBaseArray) {
            List<Object> compareList = loadJsonFile(bodyComparePath, new TypeToken<List<Object>>() {}, null);
            RuleEntry ruleEntry = extractRuleEntry(compareList);

            json = mergeList(
                ruleEntry,
                loadJsonFile(bodyBasePath.toString(),
                    new TypeToken<List<Object>>() {}, new ArrayList<>()),
                compareList
            );
        } else {
            json = mergeObject(
                loadJsonFile(bodyBasePath.toString(),
                    new TypeToken<Map<String, Object>>() {}, new HashMap<>()),
                loadJsonFile(bodyComparePath, new TypeToken<Map<String, Object>>() {}, null)
            );
        }

        OutputPathBuilder mixedBodyPathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(entry.getRequestPath())
            .setRequestName(entry.getRequestName())
            .setExtension("body");
        Path mixedBodyPath = mixedBodyPathBuilder.buildCommandPath();

        writeJsonFile(mixedBodyPath, json);

        return mixedBodyPath;
    }

    @SuppressWarnings("unchecked")
    private Object mergeObject(Map<String, Object> base, Map<String, Object> compare) {
        for(Map.Entry<String, Object> compareEntry : compare.entrySet()) {
            if(compareEntry.getValue() instanceof Map) {
                base.put(compareEntry.getKey(), mergeObject(
                    (Map<String, Object>) base.get(compareEntry.getKey()),
                    (Map<String, Object>) compareEntry.getValue() 
                ));
            } else if(compareEntry.getValue() instanceof List) {
                List<Object> list = (List<Object>) compareEntry.getValue();
                RuleEntry ruleEntry = extractRuleEntry(list);

                base.put(compareEntry.getKey(), mergeList(
                    ruleEntry, (List<Object>) base.get(compareEntry.getKey()), list
                ));
            } else {
                base.put(compareEntry.getKey(), compareEntry.getValue());
            }
        }
        return base;
    }

    private Object mergeList(RuleEntry ruleEntry, List<Object> base, List<Object> compare) {
        switch(ruleEntry.getListMergeRule()) {
            case ADD_BEGIN:
                compare.addAll(base);
                return compare;
            case ADD_END:
                base.addAll(compare);
                return base;
            case MERGE:
                return mergeList(ruleEntry.getKeyFields(), base, compare);
            default:
                return compare;
        }
    }

    @SuppressWarnings("unchecked")
    private Object mergeList(Set<String> keyFields, List<Object> base, List<Object> compare) {
        if(isEmpty(keyFields)) {
            base.addAll(compare);
            return base;
        }

        for(Object value : compare) {
            if(!(value instanceof Map)) {
                base.add(value);
                continue;
            }
            
            Map<String, Object> compareObject = (Map<String, Object>) value;
            Predicate<Map<String, Object>> targetPredicate = target ->
                keyFields
                    .stream()
                    .allMatch(key -> Objects.equals(compareObject.get(key), target.get(key)));
            Optional<Map<String, Object>> targetObject = base
                .stream()
                .map(object -> (Map<String, Object>) object)
                .filter(targetPredicate)
                .findFirst();

            if(targetObject.isPresent()) {
                mergeObject(targetObject.get(), compareObject);
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

    private Path expandBodyFile(Settings settings, Map<String, Object> defaults, MixerEntry entry,
            String extension, Path bodyPath) {
        OutputPathBuilder pathBuilder = new OutputPathBuilder(settings)
            .setRequestPath(entry.getRequestPath())
            .setRequestName(entry.getRequestName())
            .setExtension(extension);
        Path temporalBodyPath = pathBuilder.buildCommandPath();

        expandFile(settings, bodyPath, temporalBodyPath, defaults);

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
