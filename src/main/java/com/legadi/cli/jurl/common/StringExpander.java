package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.EMPTY_MAP;
import static com.legadi.cli.jurl.common.CommonUtils.INVALID_INDEX;
import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.strip;
import static com.legadi.cli.jurl.common.CommonUtils.stripEnd;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
import static com.legadi.cli.jurl.common.ObjectsRegistry.find;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByName;

import java.io.File;
import java.nio.file.Files;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.cli.jurl.exception.ConsoleInputException;
import com.legadi.cli.jurl.generators.Generator;
import com.legadi.cli.jurl.modifiers.ValueModifier;

public class StringExpander {

    private final Pattern paramTagPattern = Pattern.compile("^([\\w_]+:)?(~(.*)~)?(.*)$");
    private final Pattern paramPattern;
    private final Settings settings;
    private final PropertyDefaultResolver propertyDefaultResolver;
    private final String outputPathPart;

    public StringExpander(Settings settings) {
        this(settings, null);
    }

    public StringExpander(Settings settings, Map<String, Object> propertyDefaults) {
        this.paramPattern = Pattern.compile(settings.getSettingsParamRegex());
        this.settings = settings;
        this.propertyDefaultResolver = propertyDefaults != null
            ? new PropertyDefaultResolver(settings, propertyDefaults) : null;
        this.outputPathPart = strip(settings.getConfigOutputPath().toString(), "./");
    }

    public Settings getSettings() {
        return settings;
    }

    public String replaceAllInPath(String content) {
        return replaceAllInContent(EMPTY_MAP, content, false);
    }

    public String replaceAllInPath(Map<String, String> values, String content) {
        return replaceAllInContent(values, content, false);
    }

    public String replaceAllInContent(String content) {
        return replaceAllInContent(EMPTY_MAP, content, true);
    }

    public String replaceAllInContent(Map<String, String> values, String content) {
        return replaceAllInContent(values, content, true);
    }

    private String replaceAllInContent(Map<String, String> values, String content, boolean expandFromOutput) {
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
                    value = getValue(property, values, expandFromOutput);
                }

                if(isNotBlank(modifierDefinition)) {
                    Optional<ValueModifier> modifier = find(ValueModifier.class, modifierDefinition);

                    if(modifier.isPresent()) {
                        value = modifier.get().applyByDefinition(settings, values, modifierDefinition, value);
                    }
                }

                String paramTagEscaped = paramTag.replaceAll("\\[", "\\\\[");
                paramTagEscaped = paramTagEscaped.replaceAll("\\]", "\\\\]");

                String paramRegex = settings.getSettingsParamRegexBegin()
                    + paramTagEscaped + settings.getSettingsParamRegexEnd();

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

    private String getValue(String property, Map<String, String> values, boolean expandFromOutput) {
        String value = settings.getOrDefaultWithValues(property, values, "");

        if(expandFromOutput && isNotBlank(value)) {
            value = expandFromOutput(value);
        }

        if(propertyDefaultResolver != null && isBlank(value)) {
            return propertyDefaultResolver.apply(property);
        } else {
            return value;
        }
    }

    private String expandFromOutput(String value) {
        if(!value.contains(outputPathPart)) {
            return value;
        }
        try {
            File output = new File(value);
            if(output.exists()) {
                return new String(Files.readAllBytes(output.toPath()));
            } else {
                return value;
            }
        } catch(Exception ex) {
            return value;
        }
    }

    public static class PropertyDefaultResolver implements Function<String, String> {

        private final ConsoleInput consoleInput;
        private final Settings settings;
        private final Map<String, Object> defaults;

        public PropertyDefaultResolver(Settings settings, Map<String, Object> defaults) {
            this.consoleInput = new ConsoleInput(settings);
            this.settings = settings;
            this.defaults = defaults;
        }

        @Override
        @SuppressWarnings("unchecked")
        public String apply(String property) {
            Object defaultValue = defaults.get(property);

            if(defaultValue == null) {
                defaultValue = "";
            }

            String value;
            if(defaultValue instanceof List) {
                value = getDefault(property, (List<String>) defaultValue);
            } else {
                value = consoleInput.readInput(property, defaultValue.toString());
            }
            return saveInput(property, value);
        }

        private String getDefault(String property, List<String> defaultValues) {
            int defaultIndex = Optional.ofNullable(defaults.get(getDefaultFieldIndex(property)))
                .map(val -> (String) val)
                .filter(CommonUtils::isNumeric)
                .map(Integer::parseInt)
                .orElse(INVALID_INDEX);
            String defaultValue;

            try {
                defaultValue = defaultValues.get(defaultIndex);
            } catch(IndexOutOfBoundsException ex) {
                defaultValue = "";
            }
            try {
                String value = consoleInput.selectOption(defaultValues, defaultValue)
                    .orElse("");
                return saveInput(property, value);
            } catch(ConsoleInputException ex) {
                return "";
            }
        }

        private String saveInput(String property, String value) {
            if(isNotBlank(value)) {
                settings.putUserInput(property, value);
            }
            return value;
        }
    }
}
