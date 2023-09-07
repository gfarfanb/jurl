package com.legadi.jurl.common;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.gson.Gson;

public class StringUtils {

    private static final Gson GSON = new Gson();

    private StringUtils() {}

    public static boolean isNotBlank(String value) {
        return value != null && !value.trim().isEmpty();
    }

    public static boolean isBlank(String value) {
        return !isNotBlank(value);
    }

    public static String strip(String value, String stripChars) {
        value = stripStart(value, stripChars);
        return stripEnd(value, stripChars);
    }

    public static String stripStart(String value, String stripChars) {
        int length = (value == null ? 0 : value.length());
        if(length == 0) {
            return value;
        }
        int index = 0;
        if(stripChars == null) {
            while(index != length && Character.isWhitespace(value.charAt(index))) {
                index++;
            }
        } else if(stripChars.isEmpty()) {
            return value;
        } else {
            while(index != length && stripChars.indexOf(value.charAt(index)) != -1) {
                index++;
            }
        }
        return value.substring(index);
    }

    public static String stripEnd(String value, String stripChars) {
        int length = (value == null ? 0 : value.length());
        if(length == 0) {
            return value;
        }
        int index = length - 1;
        if(stripChars == null) {
            while(index != 0 && Character.isWhitespace(value.charAt(index))) {
                index--;
            }
        } else if(stripChars.isEmpty()) {
            return value;
        } else {
            while(index != 0 && stripChars.indexOf(value.charAt(index)) != -1) {
                index--;
            }
        }
        return value.substring(0, index);
    }

    public static String toJsonString(Object value) {
        return GSON.toJson(value);
    }

    public static String replaceAllInContent(Settings settings, String content) {
        return replaceAllInContent(settings, new HashMap<>(), content);
    }

    public static String replaceAllInContent(Settings settings, Map<String, String> values, 
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
                String value = values.getOrDefault(paramName, settings.getOrDefault(paramName, ""));

                if(isNotBlank(value)) {
                    content = content.replaceAll(paramRegex, value);
                }

                paramTags.add(paramTag);
            }
        }

        return content;
    }

    public static Set<String> scanParamsInContent(Settings settings, String content) {
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
