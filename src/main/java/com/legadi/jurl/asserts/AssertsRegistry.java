package com.legadi.jurl.asserts;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.legadi.jurl.exception.CommandException;

public class AssertsRegistry {

    private static final List<AssertFunction> ASSERTS = new LinkedList<>();
    private static final Map<String, AssertFunction> REGISTERED = new HashMap<>();

    static {

    }

    public static AssertFunction findByName(String name) {
        return ASSERTS
            .stream()
            .filter(function -> function.accepts(name))
            .findFirst()
            .orElseThrow(() -> new CommandException("Unable to obtain assert function: " + name));
    }

    @SuppressWarnings("unchecked")
    public static AssertFunction registerAssertFunction(String assertClass) {
        try {
            if(REGISTERED.containsKey(assertClass)) {
                return REGISTERED.get(assertClass);
            }

            Class<AssertFunction> type = (Class<AssertFunction>) Class.forName(assertClass);
            Constructor<AssertFunction> constructor = type.getConstructor();
            AssertFunction function = constructor.newInstance();

            ASSERTS.add(function);
            REGISTERED.put(assertClass, function);

            return function;
        } catch(Exception ex) {
            throw new IllegalStateException("Unable to instance assert function from: " + assertClass, ex);
        }
    }
}
