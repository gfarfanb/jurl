package com.legadi.cli.jurl.common;

import static com.legadi.cli.jurl.common.CommonUtils.ARGS_ESCAPED;
import static com.legadi.cli.jurl.common.CommonUtils.ARGS_REGEX;
import static com.legadi.cli.jurl.common.CommonUtils.ARGS_SEPARATOR;
import static com.legadi.cli.jurl.common.CommonUtils.EMPTY_MAP;
import static com.legadi.cli.jurl.common.CommonUtils.INVALID_INDEX;
import static com.legadi.cli.jurl.common.CommonUtils.LSQUARE_ESCAPED;
import static com.legadi.cli.jurl.common.CommonUtils.LSQUARE_REGEX;
import static com.legadi.cli.jurl.common.CommonUtils.RSQUARE_ESCAPED;
import static com.legadi.cli.jurl.common.CommonUtils.RSQUARE_REGEX;
import static com.legadi.cli.jurl.common.CommonUtils.getDefaultFieldIndex;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.stripEnd;
import static com.legadi.cli.jurl.common.CommonUtils.stripStart;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
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

    private static final Pattern PARAM_TAG_PATTERN = Pattern.compile("^([\\w_-]+" + ARGS_SEPARATOR + ")?(.*)$");

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
        int found = 0;

        while(paramMatcher.find()) {
            String paramTag = paramMatcher.group(1);
            found++;

            if(!paramTags.contains(paramTag)) {
                ParamTagDefinition paramTagDef = parseParamTag(paramTag);
                String value = null;

                if(paramTagDef.getGenerator().isPresent()) {
                    value = paramTagDef.getGenerator().get().get(settings, paramTagDef.getValue());
                }

                if(value == null) {
                    value = getValue(paramTagDef.getValue(), values,
                        paramTagDef.getModifier(), paramTagDef.getModifierArgs());
                }

                if(paramTagDef.getModifier().isPresent() && !isDefaultModifier(paramTagDef.getModifier())) {
                    value = paramTagDef.getModifier().get().applyByDefinition(settings, values,
                        paramTagDef.getModifierArgs(), value);
                }

                String paramTagEscaped = paramTag.replaceAll(LSQUARE_REGEX, LSQUARE_ESCAPED);
                paramTagEscaped = paramTagEscaped.replaceAll(RSQUARE_REGEX, RSQUARE_ESCAPED);
                paramTagEscaped = paramTagEscaped.replaceAll(ARGS_REGEX, ARGS_ESCAPED);

                String paramRegex = settings.getSettingsParamRegexBegin()
                    + paramTagEscaped + settings.getSettingsParamRegexEnd();

                content = content.replaceAll(paramRegex, value);

                paramTags.add(paramTag);
            }
        }

        if(found > 0) {
            return replaceAllInContent(values, content);
        } else {
            return content;
        }
    }

    public ParamTagDefinition parseParamTag(String paramTag) {
        Matcher paramTagMatcher = PARAM_TAG_PATTERN.matcher(paramTag);
        paramTagMatcher.find();

        String generatorName = stripEnd(paramTagMatcher.group(1), ARGS_SEPARATOR);
        Optional<Generator> generator = findByName(Generator.class, generatorName);
        String valuePart = stripStart(paramTagMatcher.group(2), ARGS_SEPARATOR);

        Matcher valueMatcher = PARAM_TAG_PATTERN.matcher(valuePart);
        valueMatcher.find();

        String modifierName = stripEnd(valueMatcher.group(1), ARGS_SEPARATOR);
        Optional<ValueModifier> modifier = findByName(ValueModifier.class, modifierName);

        if(modifier.isPresent()) {
            String modifierPart = trim(valueMatcher.group(2));
            Pair<String[], String> argsAndValue = modifier.get().extractArgsAndValue(modifierPart);
            return new ParamTagDefinition(generator, modifier, argsAndValue.getLeft(), argsAndValue.getRight());
        } else {
            return new ParamTagDefinition(generator, modifier, null, valuePart);
        }
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
            Optional<ValueModifier> modifier, String[] modifierArgs) {
        String value = settings.getOrDefaultWithValues(property, values, "");

        if(isDefaultModifier(modifier)) {
            return modifier.get().applyByDefinition(settings, values, modifierArgs, value);
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

    public static class ParamTagDefinition {

        private final Optional<Generator> generator;
        private final Optional<ValueModifier> modifier;
        private final String[] modifierArgs;
        private final String value;

        public ParamTagDefinition(Optional<Generator> generator, Optional<ValueModifier> modifier,
                String[] modifierArgs, String value) {
            this.generator = generator;
            this.modifier = modifier;
            this.modifierArgs = modifierArgs;
            this.value = value;
        }

        public Optional<Generator> getGenerator() {
            return generator;
        }

        public Optional<ValueModifier> getModifier() {
            return modifier;
        }

        public String[] getModifierArgs() {
            return modifierArgs;
        }

        public String getValue() {
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
