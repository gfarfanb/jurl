package com.legadi.jurl.modifiers;

import static com.legadi.jurl.common.CommonUtils.strip;

import java.util.Map;
import java.util.function.Function;

import com.legadi.jurl.common.Evaluable;
import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.InvalidModifierOperationException;
import com.legadi.jurl.exception.ModifierException;

public interface ValueModifier extends Evaluable {

    @Override
    default boolean accepts(String definition) {
        return definition.startsWith(name());
    }

    String name();

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
        } catch(InvalidModifierOperationException ex) {
            throw new ModifierException(name(), getArgs(), args, value, "Invalid modifier operation: " + ex.getOperation());
        } catch(Exception ex) {
            throw new ModifierException(name(), getArgs(), args, value, ex.getMessage());
        }
    }
}
