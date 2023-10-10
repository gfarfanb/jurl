package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

import com.legadi.jurl.exception.CommandException;

public class AssertionsRegistry {

    private static final List<Supplier<AssertionFunction>> ASSERTIONS = new LinkedList<>();
    private static final Map<String, Supplier<AssertionFunction>> REGISTERED = new HashMap<>();
    private static final Set<String> NAMES = new HashSet<>();

    static {
        registerAssertionFunction(ContainsAssertionFunction::new);
        registerAssertionFunction(DoesNotContainsAssertionFunction::new);
        registerAssertionFunction(DoesNotMatchAssertionFunction::new);
        registerAssertionFunction(EndsWithAssertionFunction::new);
        registerAssertionFunction(EqualsToAssertionFunction::new);
        registerAssertionFunction(GreaterThanAssertionFunction::new);
        registerAssertionFunction(GreaterThanOrEqualsToAssertionFunction::new);
        registerAssertionFunction(IsBetweenAssertionFunction::new);
        registerAssertionFunction(IsBlankAssertionFunction::new);
        registerAssertionFunction(IsFalseAssertionFunction::new);
        registerAssertionFunction(IsInAssertionFunction::new);
        registerAssertionFunction(IsNotBetweenAssertionFunction::new);
        registerAssertionFunction(IsNotBlankAssertionFunction::new);
        registerAssertionFunction(IsNotInAssertionFunction::new);
        registerAssertionFunction(IsNotNullAssertionFunction::new);
        registerAssertionFunction(IsNullAssertionFunction::new);
        registerAssertionFunction(IsTrueAssertionFunction::new);
        registerAssertionFunction(LessThanAssertionFunction::new);
        registerAssertionFunction(LessThanOrEqualsToAssertionFunction::new);
        registerAssertionFunction(MatchesAssertionFunction::new);
        registerAssertionFunction(NotEqualsToAssertionFunction::new);
        registerAssertionFunction(StartsWithAssertionFunction::new);
    }

    public static AssertionFunction registerAssertionFunction(String assertionClass) {
        if(REGISTERED.containsKey(assertionClass)) {
            return REGISTERED.get(assertionClass).get();
        } else {
            Supplier<AssertionFunction> assertionSupplier = () -> instantiate(assertionClass);
            REGISTERED.put(assertionClass, assertionSupplier);
            return assertionSupplier.get();
        }
    }

    public static void registerAssertionFunction(Supplier<AssertionFunction> assertionSupplier) {
        AssertionFunction assertionFunction = assertionSupplier.get();

        if(NAMES.contains(assertionFunction.name())) {
            throw new CommandException("Assertion function [" + assertionFunction.name() + "] already exists");
        }

        NAMES.add(assertionFunction.name());
        ASSERTIONS.add(assertionSupplier);
    }

    public static AssertionFunction findByName(String name) {
        return ASSERTIONS
            .stream()
            .map(Supplier::get)
            .filter(assertion -> assertion.name().equalsIgnoreCase(name))
            .findFirst()
            .orElseThrow(() -> new CommandException("Unable to obtain assertion function: " + name));
    }
}
