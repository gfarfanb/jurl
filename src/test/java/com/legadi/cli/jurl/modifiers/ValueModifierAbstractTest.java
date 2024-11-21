package com.legadi.cli.jurl.modifiers;

import static com.legadi.cli.jurl.common.CommonUtils.ARGS_SEPARATOR;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findByNameOrFail;
import static com.legadi.cli.jurl.common.ObjectsRegistry.findOrFail;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.cli.jurl.common.Pair;
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
        String[] args = sampleValidArgs();
        String[] invalidArgs = null;

        if(args.length > 0) {
            invalidArgs = new String[args.length - 1];
            System.arraycopy(args, 0, invalidArgs, 0, invalidArgs.length);
        }

        String validArgsDefinition = Arrays.stream(sampleValidArgs())
            .collect(Collectors.joining(ARGS_SEPARATOR));

        Pair<String[], String> argsAndValue = Assertions.assertDoesNotThrow(
            () -> modifier.extractArgsAndValue(validArgsDefinition));

        Assertions.assertEquals(args.length, argsAndValue.getLeft().length);
        Assertions.assertNotNull(argsAndValue.getRight());

        if(invalidArgs != null) {
            String invalidArgsDefinition = Arrays.stream(invalidArgs)
                .collect(Collectors.joining(ARGS_SEPARATOR));

            Assertions.assertThrows(ModifierException.class, 
                () -> modifier.extractArgsAndValue(invalidArgsDefinition));
        }
    }

    public abstract String[] sampleValidArgs();

    public String apply(String[] args, String value) {
        return modifier.applyByDefinition(settings, values, args, value);
    }
}
