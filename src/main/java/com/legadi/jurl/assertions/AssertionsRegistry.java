package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legadi.jurl.exception.CommandException;

public class AssertionsRegistry {

    private static final List<AssertionFunction> ASSERTIONS = new LinkedList<>();
    private static final Map<String, AssertionFunction> REGISTERED = new HashMap<>();

    static {
        ASSERTIONS.add(new ContainsAssertionFunction());
        ASSERTIONS.add(new DoesNotContainsAssertionFunction());
        ASSERTIONS.add(new DoesNotMatchAssertionFunction());
        ASSERTIONS.add(new EndsWithAssertionFunction());
        ASSERTIONS.add(new EqualsToAssertionFunction());
        ASSERTIONS.add(new GreaterThanAssertionFunction());
        ASSERTIONS.add(new GreaterThanOrEqualsToAssertionFunction());
        ASSERTIONS.add(new IsBetweenAssertionFunction());
        ASSERTIONS.add(new IsBlankAssertionFunction());
        ASSERTIONS.add(new IsFalseAssertionFunction());
        ASSERTIONS.add(new IsInAssertionFunction());
        ASSERTIONS.add(new IsNotBetweenAssertionFunction());
        ASSERTIONS.add(new IsNotBlankAssertionFunction());
        ASSERTIONS.add(new IsNotInAssertionFunction());
        ASSERTIONS.add(new IsNotNullAssertionFunction());
        ASSERTIONS.add(new IsNullAssertionFunction());
        ASSERTIONS.add(new IsTrueAssertionFunction());
        ASSERTIONS.add(new LessThanAssertionFunction());
        ASSERTIONS.add(new LessThanOrEqualsToAssertionFunction());
        ASSERTIONS.add(new MatchesAssertionFunction());
        ASSERTIONS.add(new NotEqualsToAssertionFunction());
        ASSERTIONS.add(new StartsWithAssertionFunction());
    }

    public static AssertionFunction registerAssertionFunction(String assertionClass) {
        if(REGISTERED.containsKey(assertionClass)) {
            return REGISTERED.get(assertionClass);
        } else {
            AssertionFunction function = instantiate(assertionClass);
            ASSERTIONS.add(function);
            REGISTERED.put(assertionClass, function);
            return function;
        }
    }

    public static AssertionFunction findByName(String name) {
        return ASSERTIONS
            .stream()
            .filter(function -> function.name().equalsIgnoreCase(name))
            .findFirst()
            .orElseThrow(() -> new CommandException("Unable to obtain assertion function: " + name));
    }
}
