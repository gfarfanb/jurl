package com.legadi.jurl.common;

import java.io.Console;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;

public class StringExpander {

    private static final String INPUT_PREFIX = "INPUT:";
    private static final String PASSWORD_PREFIX = "PASSWORD:";

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
        Console console = System.console();

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
                String value;

                if(paramName.startsWith(INPUT_PREFIX)) {
                    value = console.readLine(extractMessage(INPUT_PREFIX, paramName));
                } else if(paramName.startsWith(PASSWORD_PREFIX)) {
                    value = String.valueOf(
                        console.readPassword(extractMessage(PASSWORD_PREFIX, paramName))
                    );
                } else {
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

    private String extractMessage(String prefix, String value) {
        return value.substring(prefix.length(), value.length());
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
