package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.EMPTY_MAP;
import static com.legadi.cli.jurl.common.CommonUtils.INVALID_INDEX;
import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.stripEnd;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
import static com.legadi.cli.jurl.common.ObjectsRegistry.find;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByName;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.cli.jurl.exception.ConsoleInputException;
import com.legadi.cli.jurl.generators.Generator;
import com.legadi.cli.jurl.modifiers.DefaultValueModifier;
import com.legadi.cli.jurl.modifiers.ValueModifier;

public class StringExpander {

    private static final Logger LOGGER = Logger.getLogger(StringExpander.class.getName());

    private final Pattern paramTagPattern = Pattern.compile("^([\\w_]+:)?(~(.*)~)?(.*)$");
    private final Pattern paramPattern;
    private final Settings settings;
    private final PropertyDefaultResolver propertyDefaultResolver;

    public StringExpander(Settings settings) {
        this(settings, null);
    }

    public StringExpander(Settings settings, Map<String, Object> propertyDefaults) {
        this.paramPattern = Pattern.compile(settings.getSettingsParamRegex());
        this.settings = settings;
        this.propertyDefaultResolver = propertyDefaults != null
            ? new PropertyDefaultResolver(settings, propertyDefaults) : null;
    }

    public Settings getSettings() {
        return settings;
    }

    public String replaceAllInContent(String content) {
        return replaceAllInContent(EMPTY_MAP, content);
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
                Optional<ValueModifier> modifier;
                String value = null;

                if(generator.isPresent()) {
                    value = generator.get().get(settings, property);
                }

                if(isNotBlank(modifierDefinition)) {
                    modifier = find(ValueModifier.class, modifierDefinition);
                } else {
                    modifier = Optional.empty();
                }

                if(value == null) {
                    value = getValue(property, values, modifier, modifierDefinition);
                }

                if(modifier.isPresent() && !isDefaultModifier(modifier)) {
                    value = modifier.get().applyByDefinition(settings, values, modifierDefinition, value);
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

    private String getValue(String property, Map<String, String> values,
            Optional<ValueModifier> modifier, String modifierDefinition) {
        String value = settings.getOrDefaultWithValues(property, values, "");

        if(isDefaultModifier(modifier)) {
            return modifier.get().applyByDefinition(settings, values, modifierDefinition, value);
        }

        if(propertyDefaultResolver != null && isBlank(value)) {
            return propertyDefaultResolver.apply(property);
        } else {
            return value;
        }
    }

    private boolean isDefaultModifier(Optional<ValueModifier> modifier) {
        return modifier.isPresent() && modifier.get().getClass() == DefaultValueModifier.class;
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

                LOGGER.info("");

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
