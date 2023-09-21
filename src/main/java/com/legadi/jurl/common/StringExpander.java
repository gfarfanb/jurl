package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.generators.GeneratorsRegistry.getValueByGenerator;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringExpander {

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

    public String replaceAllInContent(Map<String, String> values, 
            String content) {
        Pattern pattern = Pattern.compile(settings.getSettingsParamRegex());
        Matcher matcher = pattern.matcher(content);
        Set<String> paramTags = new HashSet<>();

        while(matcher.find()) {
            String paramTag = matcher.group(0);

            if(!paramTags.contains(paramTag)) {
                String paramName = paramTag.substring(
                    settings.getSettingsParamStartAt(),
                    paramTag.length() - settings.getSettingsParamEndAtLengthMinus()
                );
                String paramRegex = settings.getSettingsParamRegexMask().replace(
                    settings.getSettingsParamRegexReplace(), paramName
                );
                String value = getValueByGenerator(settings, paramName);

                if(value == null) {
                    value = values.getOrDefault(paramName,
                        settings.getOrDefault(paramName, ""));
                }

                if(isNotBlank(value)) {
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
