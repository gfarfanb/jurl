package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.CommonUtils.ARGS_ESCAPED;
import static com.legadi.cli.jurl.common.CommonUtils.ARGS_SEPARATOR;
import static com.legadi.cli.jurl.common.CommonUtils.isBlank;
import static com.legadi.cli.jurl.common.CommonUtils.strip;

import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.legadi.cli.jurl.common.Evaluable;
import com.legadi.cli.jurl.common.Named;
import com.legadi.cli.jurl.common.Pair;
import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.ModifierException;

public interface ValueModifier extends Evaluable, Named {

    @Override
    default boolean accepts(String definition) {
        return definition.toLowerCase().startsWith(name().toLowerCase());
    }

    @Override
    default boolean allowOverride() {
        return false;
    }

    String[] getArgs();

    String apply(Function<String, String> propertyResolver, String[] args, String value) throws Exception;

    default Pair<String[], String> extractArgsAndValue(String definition) {
        String separatorTag = UUID.randomUUID().toString();

        definition = strip(definition, ARGS_SEPARATOR)
            .replaceAll(ARGS_ESCAPED, separatorTag);

        String[] parts = isBlank(definition) ? new String[0] : definition.split(ARGS_SEPARATOR);

        IntStream.range(0, parts.length)
            .forEach(i -> parts[i] = parts[i].replaceAll(separatorTag, ARGS_SEPARATOR));

        String[] args = new String[getArgs().length];

        try {
            System.arraycopy(parts, 0, args, 0, args.length);
        } catch(IndexOutOfBoundsException ex) {
            throw new ModifierException(name(), getArgs(), parts, null, "Invalid number of arguments for modifier");
        }

        String value = IntStream.range(args.length, parts.length)
            .mapToObj(i -> parts[i])
            .collect(Collectors.joining(ARGS_SEPARATOR));

        return new Pair<>(args, value);
    }

    default String applyByDefinition(Settings settings, Map<String, String> values,
            String[] args, String value) {
        try {
            Function<String, String> propertyResolver = property -> values.getOrDefault(property,
                settings.getOrDefault(property, property));

            return apply(propertyResolver, args, value);
        } catch(Exception ex) {
            throw new ModifierException(name(), getArgs(), args, value, ex.getMessage());
        }
    }
}
