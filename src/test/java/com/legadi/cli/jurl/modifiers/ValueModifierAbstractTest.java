package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.exception.ModifierException;

public abstract class ValueModifierAbstractTest<T extends ValueModifier> {

    private final Settings settings;
    private final Map<String, String> values;
    private final String name;
    private final T modifier;

    public ValueModifierAbstractTest(String name) {
        this.settings = new Settings();
        this.values = new HashMap<>();
        this.name = name;
        this.modifier = findByNameOrFail(ValueModifier.class, name);
    }

    @Test
    public void acceptsValidation() {
        Assertions.assertDoesNotThrow(() -> findOrFail(ValueModifier.class, name));
    }

    @Test
    public void expectedArgs() {
        String[] args = modifier.getArgs();

        if(args.length > 0) {
            String[] def = new String[args.length - 1];

            System.arraycopy(args, 1, def, 0, def.length);

            String definition = String.join("~", def);

            Assertions.assertThrows(ModifierException.class, 
                () -> modifier.applyByDefinition(settings, values, definition, null));
        }
    }

    public String apply(String definition, String value) {
        return modifier.applyByDefinition(settings, values, definition, value);
    }
}
