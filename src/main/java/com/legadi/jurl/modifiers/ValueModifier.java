package com.legadi.jurl.modifiers;

import com.legadi.jurl.common.Settings;

public interface ValueModifier {

    String name();

    default boolean accepts(String definition) {
        return definition.startsWith(name());
    }

    String apply(Settings settings, String definition, String value);
}
