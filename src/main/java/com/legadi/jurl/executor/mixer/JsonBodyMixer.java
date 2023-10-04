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

import com.google.gson.reflect.TypeToken;
import com.legadi.jurl.common.OutputPathBuilder;
import com.legadi.jurl.common.Settings;

public class JsonBodyMixer implements BodyMixer {

    public enum ListRule {

        REPLACE, ADD_BEGIN, ADD_END
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
            ListRule listRule = extractListRule(list);

            json = mergeList(
                listRule,
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
                ListRule listRule = extractListRule(list);

                base.put(updateEntry.getKey(), mergeList(
                    listRule, (List<Object>) base.get(updateEntry.getKey()), list
                ));
            } else {
                base.put(updateEntry.getKey(), updateEntry.getValue());
            }
        }
        return base;
    }

    private Object mergeList(ListRule listRule, List<Object> base, List<Object> update) {
        switch(listRule) {
            case ADD_BEGIN:
                update.addAll(base);
                return update;
            case ADD_END:
                base.addAll(update);
                return base;
            default:
                return update;
        }
    }

    private ListRule extractListRule(List<Object> list) {
        if(isEmpty(list)) {
            return ListRule.REPLACE;
        }

        try {
            String ruleElementRaw = toJsonString(list.get(0));
            RuleEntry ruleEntry = jsonToObject(ruleElementRaw, new TypeToken<RuleEntry>() {});
            list.remove(0);
            return ruleEntry.getListMergeRule();
        } catch(IllegalStateException ex) {
            return ListRule.REPLACE;
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

    public static class RuleEntry {

        private ListRule listMergeRule;

        public ListRule getListMergeRule() {
            return listMergeRule;
        }

        public void setListMergeRule(ListRule listMergeRule) {
            this.listMergeRule = listMergeRule;
        }
    }
}
