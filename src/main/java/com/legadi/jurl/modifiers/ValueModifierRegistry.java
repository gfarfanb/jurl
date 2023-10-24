package com.legadi.jurl.modifiers;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;

public class ValueModifierRegistry {

    private static final List<Pair<Predicate<String>, Supplier<ValueModifier>>> MODIFIERS = new LinkedList<>();

    static {
        registerModifier(DateTimeValueModifier::new);
        registerModifier(NumberValueModifier::new);
        registerModifier(StringValueModifier::new);
    }

    private ValueModifierRegistry() {}

    public static void registerModifier(String modifierClass) {
        registerModifier(() -> instantiate(modifierClass));
    }

    public static void registerModifier(Supplier<ValueModifier> modifierSupplier) {
        ValueModifier modifier = modifierSupplier.get();
        MODIFIERS.add(new Pair<>(definition -> modifier.accepts(definition), modifierSupplier));
    }

    public static ValueModifier findModifierByDefinition(String definition) {
        List<ValueModifier> modifiers = MODIFIERS
            .stream()
            .filter(p -> p.getLeft().test(definition))
            .map(Pair::getRight)
            .map(Supplier::get)
            .collect(Collectors.toCollection(ArrayList::new));

        if(modifiers.isEmpty()) {
            return null;
        }

        ValueModifier lastModifier = modifiers.get(modifiers.size() - 1);
        return lastModifier;
    }
}
