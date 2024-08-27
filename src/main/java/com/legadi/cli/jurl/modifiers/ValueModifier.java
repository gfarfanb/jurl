package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.CommonUtils.strip;

import java.util.Map;
import java.util.function.Function;

import com.legadi.cli.jurl.common.Evaluable;
import com.legadi.cli.jurl.common.Named;
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

    String apply(Function<String, String> getter, String[] args, String value) throws Exception;

    default String applyByDefinition(Settings settings, Map<String, String> values,
            String definition, String value) {
        String[] input = strip(definition, "~").split("~");
        String[] args = new String[input.length - 1];

        System.arraycopy(input, 1, args, 0, args.length);

        if(args.length < getArgs().length) {
            throw new ModifierException(name(), getArgs(), args, value, "Invalid number of arguments for modifier");
        }

        try {
            Function<String, String> getter = property -> values.getOrDefault(property,
                settings.getOrDefault(property, property));

            return apply(getter, args, value);
        } catch(Exception ex) {
            throw new ModifierException(name(), getArgs(), args, value, ex.getMessage());
        }
    }
}
