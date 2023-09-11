package com.legadi.jurl.assertions;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legadi.jurl.exception.CommandException;

public class AssertionsRegistry {

    private static final List<AssertionFunction> ASSERTIONS = new LinkedList<>();
    private static final Map<String, AssertionFunction> REGISTERED = new HashMap<>();

    static {

    }

    public static AssertionFunction findByName(String name) {
        return ASSERTIONS
            .stream()
            .filter(function -> function.accepts(name))
            .findFirst()
            .orElseThrow(() -> new CommandException("Unable to obtain assertion function: " + name));
    }

    @SuppressWarnings("unchecked")
    public static AssertionFunction registerAssertFunction(String assertionClass) {
        try {
            if(REGISTERED.containsKey(assertionClass)) {
                return REGISTERED.get(assertionClass);
            }

            Class<AssertionFunction> type = (Class<AssertionFunction>) Class.forName(assertionClass);
            Constructor<AssertionFunction> constructor = type.getConstructor();
            AssertionFunction function = constructor.newInstance();

            ASSERTIONS.add(function);
            REGISTERED.put(assertionClass, function);

            return function;
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to instance assertion function from: " + assertionClass, ex);
        }
    }
}
