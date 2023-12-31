package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.stripEnd;
import static com.legadi.jurl.common.CommonUtils.trim;
import static com.legadi.jurl.common.ObjectsRegistry.find;
import static com.legadi.jurl.common.ObjectsRegistry.findByName;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.jurl.generators.Generator;
import com.legadi.jurl.modifiers.ValueModifier;

public class StringExpander {

    private final Pattern paramTagPattern = Pattern.compile("^([\\w_]+:)?(~(.*)~)?(.*)$");
    private final Pattern paramPattern;
    private final Settings settings;

    public StringExpander(Settings settings) {
        this.paramPattern = Pattern.compile(settings.getSettingsParamRegex());
        this.settings = settings;
    }

    public Settings getSettings() {
        return settings;
    }

    public String replaceAllInContent(String content) {
        return replaceAllInContent(new HashMap<>(), content);
    }

    public String replaceAllInContent(Map<String, String> values, String content) {
        if(content == null) {
            return null;
        }

        Matcher paramMatcher = paramPattern.matcher(content);
        Set<String> paramTags = new HashSet<>();

        while(paramMatcher.find()) {
            String paramTag = paramMatcher.group(1);

            if(!paramTags.contains(paramTag)) {
                Matcher paramTagMatcher = paramTagPattern.matcher(paramTag);

                paramTagMatcher.find();

                Optional<Generator> generator = findByName(Generator.class, stripEnd(paramTagMatcher.group(1), ":"));
                String modifierDefinition = paramTagMatcher.group(3);
                String property = trim(paramTagMatcher.group(4));
                String value = null;

                if(generator.isPresent()) {
                    value = generator.get().get(settings, property);
                }

                if(value == null) {
                    value = settings.getOrDefaultWithValues(property, values, "");
                }

                if(isNotBlank(modifierDefinition)) {
                    Optional<ValueModifier> modifier = find(ValueModifier.class, modifierDefinition);

                    if(modifier.isPresent()) {
                        value = modifier.get().applyByDefinition(settings, values, modifierDefinition, value);
                    }
                }

                String paramRegex = settings.getSettingsParamRegexBegin()
                    + paramTag + settings.getSettingsParamRegexEnd();

                content = content.replaceAll(paramRegex, value);

                paramTags.add(paramTag);
            }
        }

        return content;
    }

    public Set<String> scanParamsInContent(String content) {
        Pattern pattern = Pattern.compile(settings.getSettingsParamRegex());
        Matcher matcher = pattern.matcher(content);
        Set<String> paramNames = new HashSet<>();

        while(matcher.find()) {
            String paramTag = matcher.group(1);

            paramNames.add(paramTag);
        }

        return paramNames;
    }
}
