package com.legadi.jurl.modifiers;

import static com.legadi.jurl.common.CommonUtils.strip;

import com.legadi.jurl.common.Settings;
import com.legadi.jurl.exception.InvalidModifierOperationException;
import com.legadi.jurl.exception.ModifierException;

public interface ValueModifier {

    String name();

    String[] getArgs();

    default boolean accepts(String definition) {
        return definition.startsWith(name());
    }

    String apply(Settings settings, String[] args, String value) throws Exception;

    default String applyByDefinition(Settings settings, String definition, String value) {
        String[] input = strip(definition, "|").split("|");
        String[] args = new String[input.length - 1];

        System.arraycopy(input, 1, args, 0, args.length);

        if(args.length < getArgs().length) {
            throw new ModifierException(name(), getArgs(), args, "Invalid number of arguments for modifier");
        }

        try {
            return apply(settings, args, value);
        } catch(InvalidModifierOperationException ex) {
            throw new ModifierException(name(), getArgs(), args, "Invalid modifier operation: " + ex.getOperation());
        } catch(Exception ex) {
            throw new ModifierException(name(), getArgs(), args, ex.getMessage());
        }
    }
}
