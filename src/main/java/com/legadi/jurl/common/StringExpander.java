package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.CommonUtils.stripEnd;
import static com.legadi.jurl.common.CommonUtils.trim;
import static com.legadi.jurl.generators.GeneratorsRegistry.findGeneratorByName;
import static com.legadi.jurl.modifiers.ValueModifierRegistry.findModifierByDefinition;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.generators.Generator;
import com.legadi.jurl.modifiers.ValueModifier;

public class StringExpander {

    private final Pattern paramPattern = Pattern.compile("^([\\w_]+:)?(\\[[\\w:.-_/]+\\])?(.*)$");
    private final Settings settings;

    public StringExpander(Settings settings) {
        this.settings = settings;
    }

    public Settings getSettings() {
        return settings;
    }

    public String replaceAllInContent(String content) {
        return replaceAllInContent(new HashMap<>(), content);
    }

    public String replaceAllInContent(Map<String, String> values, String content) {
        Pattern pattern = Pattern.compile(settings.getSettingsParamRegex());
        Matcher paramMatcher = pattern.matcher(content);
        Set<String> paramTags = new HashSet<>();

        while(paramMatcher.find()) {
            String paramTag = paramMatcher.group(0);

            if(!paramTags.contains(paramTag)) {
                String paramName = paramTag.substring(
                    settings.getSettingsParamStartAt(),
                    paramTag.length() - settings.getSettingsParamEndAtLengthMinus()
                );
                Matcher paramNameMatcher = paramPattern.matcher(paramName);

                if(!paramNameMatcher.find()) {
                    throw new CommandException("Parameter is wrong defined: " + paramName
                        + " - expected \"<generator>:?[<modifier-definition>]?<property-name>\"");
                }

                Generator generator = findGeneratorByName(stripEnd(paramNameMatcher.group(1), ":"));
                String modifierDefinition = strip(paramNameMatcher.group(2), "[]");
                String property = trim(paramNameMatcher.group(3));
                String value = null;

                if(generator != null) {
                    value = generator.get(settings, property);
                }

                if(value == null) {
                    value = values.getOrDefault(property,
                        settings.getOrDefault(property, ""));
                }

                if(isNotBlank(modifierDefinition)) {
                    ValueModifier modifier = findModifierByDefinition(modifierDefinition);

                    if(modifier != null) {
                        value = modifier.applyByDefinition(settings, modifierDefinition, value);
                    }
                }

                if(isNotBlank(value)) {
                    String paramRegex = settings.getSettingsParamRegexMask().replace(
                        settings.getSettingsParamRegexReplace(), paramName
                    );

                    content = content.replaceAll(paramRegex, value);
                }

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
            String paramTag = matcher.group(0);
            String paramName = paramTag.substring(
                settings.getSettingsParamStartAt(),
                paramTag.length() - settings.getSettingsParamEndAtLengthMinus()
            );

            paramNames.add(paramName);
        }

        return paramNames;
    }
}
